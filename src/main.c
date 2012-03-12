/*
    Copyright (c) 2005 Simon Naunton
    
    A lot of the coded needed to download a Suunto dive computer was 
    cribbed from Andreas Beck's vyperlink, so...

    Copyright (c) 2002 Andreas Beck
    Copyright (c) 2002 Mike Brodbelt
	
	...and lessons learned about the importance of setting DTR and RTS
	correctly were garnered from Uwe Ohse's mosquitoget.
	
    This file is part of gdivelog.

    gdivelog is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    gdivelog is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with gdivelog; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <glib/gi18n.h>
#include <glib/gprintf.h>
#include <gtk/gtk.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/termios.h>
#include <string.h>
#include <stdlib.h>
/* for strptime prototype - see man strptime */
#ifndef __USE_XOPEN
#define __USE_XOPEN
#endif
#include <time.h>

#include <sqlite3.h>

#include <gdivelog.h>

#define SUUNTO_DIVE_FIRST 0x08
#define SUUNTO_DIVE_NEXT 0x09
#define TIMEOUT 500000
#define NOT_SERIAL_PORT -2

typedef enum {
  SUUNTO_MODEL_UNKNOWN=0,
  SUUNTO_MODEL_OLD_SPYDER,
  SUUNTO_MODEL_NEW_SPYDER,
  SUUNTO_MODEL_OLD_COBRA_OR_VYPER,
  SUUNTO_MODEL_NEW_VYPER,
  SUUNTO_MODEL_VYTEC,
  SUUNTO_MODEL_STINGER,
  SUUNTO_MODEL_MOSQUITO
}SuuntoModel;

#define IS_SPYDER(model) (model==SUUNTO_MODEL_OLD_SPYDER||model==SUUNTO_MODEL_OLD_SPYDER)

enum {
  SUUNTO_COL_DATE=0,
  SUUNTO_COL_TIME,
  SUUNTO_COL_MAXDEPTH,
  SUUNTO_COL_DURATION,
  SUUNTO_COL_MAXTEMP,
  SUUNTO_COL_MINTEMP,
  SUUNTO_COL_DELETED,
  SUUNTO_COL_DATA,
  SUUNTO_NUM_COLS
};

typedef enum {
  DTR_STATUS_ON=0,
  DTR_STATUS_OFF
}DTRStatus;

typedef enum {
  RTS_STATUS_ON=0,
  RTS_STATUS_OFF
}RTSStatus;

typedef struct {
  gdouble O2;
  gdouble start_pressure;
  gdouble end_pressure;
  gdouble max_temperature;
  gdouble min_temperature;
  gdouble max_depth;
  glong duration;
  gchar *datetime;
  GArray *profile;
}SuuntoDive;

typedef struct {
  GtkWidget *window;
  GtkWidget *new_dives;
  GtkWidget *all_dives;
  GtkWidget *select_dives;
  GtkWidget *ok_btn;
  GtkWidget *device;
  GtkListStore *device_list_store;
  gboolean cancel_download;
  gboolean download_in_progress;
}SuuntoDownloadWindowData;

typedef struct {
  GtkWidget *window;
  GtkWidget *dive_list;
  GtkTreeModel *dive_list_model;
  GtkWidget *import_btn;
  GtkWidget *progressbar;
  GtkWidget *select_all_btn;
  GtkWidget *unselect_all_btn;
  gboolean cancel_import;
  gboolean import_in_progess;
  GArray *dives_array;
  SuuntoModel model;
}SuuntoSelectDivesWindowData;

struct termios old_termios;
static gboolean ifacealwaysechos;
static gboolean break_prof_read_early=TRUE;
static GDiveLogPluginInterface *plugin_interface;
static GtkProgressBar *progressbar;

static gchar *suunto_model_names[]={
  N_("Unknown"),
  N_("Spyder (old)"),
  N_("Spyder (new)"),
  N_("Cobra/Vyper (old)"),
  N_("Vyper (new)"),
  N_("Vytec"),
  N_("Stinger"),
  N_("Mosquito")
};

static void suunto_load_devices_liststore(GtkListStore *suunto_device_list_store, GtkComboBox *suunto_device_combobox)
{
  gchar **suunto_devices,*env;
  gint i=0;
  GtkTreeIter iter;

  suunto_device_list_store=gtk_list_store_new(1,G_TYPE_STRING);
  env=(gchar*)g_getenv("SUUNTO_DEVICE");
  if(env) {
    suunto_devices=g_strsplit(env,":",0);
    while(suunto_devices[i]) {
      gtk_list_store_append(GTK_LIST_STORE(suunto_device_list_store),&iter);
      gtk_list_store_set(GTK_LIST_STORE(suunto_device_list_store),&iter,0,suunto_devices[i],-1);
      i++;
    }
    g_strfreev(suunto_devices);   
  }	
  gtk_combo_box_set_model(suunto_device_combobox,GTK_TREE_MODEL(suunto_device_list_store));
  g_object_unref(suunto_device_list_store);
  gtk_combo_box_set_active(suunto_device_combobox,0);
}

static gboolean set_rts(gint fd,RTSStatus rts_status) 
{
  gint bits=TIOCM_RTS;

  if(ioctl(fd,rts_status==RTS_STATUS_ON?TIOCMBIS:TIOCMBIC,&bits)==-1) return FALSE;
  return TRUE;
}

static gboolean set_dtr(gint fd,DTRStatus dtr_status) 
{
  gint bits;
	
  if(ioctl(fd,TIOCMGET,&bits)) return FALSE;
  if(dtr_status==DTR_STATUS_ON) bits|=TIOCM_DTR;
  else bits&=TIOCM_DTR;
  if(ioctl(fd,TIOCMSET,&bits)) return FALSE;
  return TRUE;
}

static gint suunto_open(const char *device) 
{
  struct termios settings;
  gint fd;

  fd=open(device,O_RDWR|O_NOCTTY|O_NDELAY);
  if(fd>0) {
    if(isatty(fd)) {
      fcntl(fd,F_SETFL,0);
      tcgetattr(fd,&old_termios);
      tcflush(fd, TCIOFLUSH);
      settings.c_cflag=CS8|CREAD|PARENB|PARODD|CLOCAL;
      settings.c_iflag=IGNBRK|IGNPAR;
      settings.c_lflag=0;
      settings.c_oflag=0;
      settings.c_cc[VMIN]=0;
      settings.c_cc[VTIME]=0;
      if(!cfsetspeed(&settings,B2400)) {
        if(tcsetattr(fd,TCSANOW,&settings)!=-1) {
          if(set_dtr(fd,DTR_STATUS_ON) && set_rts(fd,RTS_STATUS_OFF)) {
            usleep(100000);
            return fd;
          }
        }
		else g_printerr("Failed to set terminal attributes.");
      }
    }
    else {
      close(fd);
      return NOT_SERIAL_PORT;
    }
  }
  return -1;
}

static void suunto_close(gint fd) 
{
  set_dtr(fd,DTR_STATUS_OFF);
  tcsetattr(fd,TCSANOW,&old_termios);
  close(fd);
}

static gboolean suunto_send_testcmd(gint fd,gchar *cmd) {

  gint len;
  gboolean rval=FALSE;
  
  len=strlen(cmd);
  if(write(fd,cmd,len)==len) rval=TRUE;
  tcdrain(fd);
  
  return rval;
}

#define suunto_read_serial(x) suunto_read_serial_with_timeoutmod(x,0)

static gint suunto_read_serial_with_timeoutmod(gint fd,gint timeoutmod) 
{
  guchar rval;
  fd_set fds;
  struct timeval tv;
	
  FD_ZERO(&fds);
  FD_SET(fd,&fds);
  tv.tv_sec=timeoutmod;
  tv.tv_usec=TIMEOUT;
  if(select(fd+1,&fds,NULL,NULL,&tv)==1) {
    read(fd,&rval,sizeof(rval));
    return rval;
  }
  return -1;
}

static gint suunto_write_serial(gint fd,guchar *buffer,gint len) 
{
  gint rc;

  set_rts(fd,RTS_STATUS_ON);
  rc=write(fd,buffer,len);
  tcdrain(fd);
  usleep(200000);
  set_rts(fd,RTS_STATUS_OFF);
  return rc;
}

static gboolean suunto_detect_interface(gint fd) 
{
  int rc=0;
  gboolean detected=TRUE,rval=FALSE;
	
  set_rts(fd,RTS_STATUS_ON);
  usleep(300000);
  set_rts(fd,RTS_STATUS_OFF);
  if(suunto_send_testcmd(fd,"AT\r")) {

    if(suunto_read_serial(fd)!='A' || suunto_read_serial(fd)!='T' || suunto_read_serial(fd) !='\r') {
      g_printf(_("Interface not responding in probe mode"));
	  detected=FALSE;
    }

/*
    suunto_read_serial(fd);
    suunto_read_serial(fd);
    suunto_read_serial(fd);
*/

    rc=suunto_read_serial(fd);
	if(rc!=-1) {
      g_printf(_("Extraneous character. Is line connected to a modem?"));
    }

    set_rts(fd,RTS_STATUS_ON);	/* Try transfer mode now */
    if(!suunto_send_testcmd(fd,"AT\r")) g_printerr(_("Cannot detect Suunto interface.")); 
    else {
	/* From dt_ab, http://www.acs.uni-duesseldorf.de/~becka/dive/divetools/index.html:
	 * This delay seems to be required by many USB-serial converters.
	 * Does not seem to cause other problems, so I put it in.
	 * Sumbitted by Christian Rüb to vyperlink. Thanks.
	 */
	  usleep(300000);
      rval=TRUE;	
      set_rts(fd,RTS_STATUS_OFF);
      rc=suunto_read_serial(fd);
      if(rc==-1) {

        if(detected) {	  
          g_printf(_("Original Suunto interface found\n"));
        }
        else {		  
		  g_printf(_("Original Sunnto interface with DC already attached\n"));
	    }

        ifacealwaysechos=FALSE;
      }
      else {

        if(rc!='A' || suunto_read_serial(fd)!='T' || suunto_read_serial(fd)!='\r') {
          g_printf(_("Interface not responding when RTS is on.\n"));
        }
        if(suunto_read_serial(fd)!=-1) {
          g_printf(_("Clone interface without RTS-switching\n"));
        }

        suunto_read_serial(fd);
        suunto_read_serial(fd);
        suunto_read_serial(fd);
        ifacealwaysechos=TRUE;
      }
    }
  }
  return rval;
}

static guchar suunto_generate_crc(guchar *buffer,gint len) 
{
  guchar crc=0;

  while(len--) crc^=*buffer++;
  return crc;
}

static gboolean suunto_send_command(gint fd,guchar *commbuffer,gint len) 
{
  gint rc;
  gboolean rval=TRUE;

  usleep(500000);
  suunto_write_serial(fd,commbuffer,len);
  if(ifacealwaysechos) {
    while(len--) {
      rc=suunto_read_serial(fd);	/* these are echos - should be there instantly */
      if(rc==-1 || rc!=*commbuffer++) rval=FALSE;
    }
  }
  return rval;
}

static int suunto_read(gint fd,gint start,guchar *retbuffer,gint len) 
{

  guchar command[]={0x05,
                    0x00, /* high  */
                    0x00, /* low   */
                    0x01, /* count */
                    0x00  /* CRC   */
                   },
         reply[4]={0,0,0,0},
         crc=0x00;
  gint i;
  gboolean rval=FALSE; 


  command[1]=(start>>8)&0xff;
  command[2]=(start)&0xff;
  command[3]=len;
  command[4]=suunto_generate_crc(command,4);

  if(suunto_send_command(fd,command,5)) {
    reply[0]=suunto_read_serial_with_timeoutmod(fd,1);
    reply[1]=suunto_read_serial(fd);
    reply[2]=suunto_read_serial(fd);
    reply[3]=suunto_read_serial(fd);

    if(reply[0]==command[0] && reply[1]==command[1] && reply[2]==command[2] && reply[3]==command[3]) {
      crc=command[0]^command[1]^command[2]^command[3];
      for(i=0;i<len;i++) {
        retbuffer[i]=suunto_read_serial(fd);
        crc^=retbuffer[i];
      }
      if(crc==suunto_read_serial(fd)) rval=TRUE;
      else g_printerr(_("CRC check failure.\n"));
    }
    else if(reply[0]==255) {
      g_printerr(_("The interface appears to be present, but the dive computer is not responding.\n\nPlease the check connections and try again."));
    }
  }
  return rval;
}

SuuntoModel suunto_get_model(gint fd)
{
  SuuntoModel model=SUUNTO_MODEL_UNKNOWN;
  guchar rbuf[2];

  if(suunto_read(fd,0x24,rbuf,1)) {   
    switch(rbuf[0]) {
      case 40 :
        if(suunto_read(fd,0x16,rbuf,2)) {
          if(rbuf[0]==0x01&&rbuf[1]==0x01) model=SUUNTO_MODEL_OLD_SPYDER;
          else if(rbuf[0]==0x01&&rbuf[1]==0x02) model=SUUNTO_MODEL_NEW_SPYDER;
        }
        break;
      case 0x0c:
        model=SUUNTO_MODEL_OLD_COBRA_OR_VYPER;
        break;
      case 0x0a:
        model=SUUNTO_MODEL_NEW_VYPER;
        break;
      case 0x0b:
        model=SUUNTO_MODEL_VYTEC;
        break;
      case 0x03:
        model=SUUNTO_MODEL_STINGER;
        break;
      case 0x04:
        model=SUUNTO_MODEL_MOSQUITO;
        break;
    }
  } else {
	g_printerr(_("Could not read mode byte\n"));
  }
  return model;
}

static gint suunto_get_dive(gint fd,guchar suunto_dive_which,guchar *divebuf,gint len) 
{
  guchar command[]={0x08,0xa5,0x00};
  gint i,j,rc,crc,packet_len;

  command[0]=(guchar)suunto_dive_which;
  command[2]=suunto_generate_crc(command,2);

  if(suunto_send_command(fd,command,3)) {
    for(i=0;i<len;) {
      rc=suunto_read_serial_with_timeoutmod(fd,2);
      if(rc!=command[0]) {
        if(rc==-1) break;
        g_printerr(_("Illegal start of packet."));
        return -1;
      }
      crc=rc;
      packet_len=suunto_read_serial(fd);
      if(packet_len<0 || packet_len>32) {
        g_printerr(_("Illegal packet length."));
         return -1;
      }
      crc^=packet_len;
      for(j=0;j<packet_len&&i<len;j++,i++) {
        rc=suunto_read_serial(fd);
        if(rc==-1) {
          g_printerr(_("Unexpected end of packet"));
          return -1;
        }
        divebuf[i]=(guchar)rc;
        crc^=divebuf[i];
      }
      crc^=suunto_read_serial(fd);
      if(crc!=0) {
        g_printerr(_("CRC check failure."));
        return -1;
      }
      if(break_prof_read_early && packet_len!=32) break; 
    }
  }
  else {
    g_printerr(_("Download failed."));
    return -1;
  }
  return i;
}

static gboolean suunto_parse_dive(guchar *divebuf,gint len,SuuntoModel model,gchar *last_dive_datetime,GArray *dive_array)
{
  gint j,i,interval=1;
  gdouble depth=0.0;
  ImportProfile profile_segment,*profile_segmentptr;
  SuuntoDive dive;
  guint8 lwr=0x79,upr=0x87; /* profile bytes between lwr & upr are alarms, not depth deltas */
  
  i=len-1;
  if(IS_SPYDER(model)) {
    lwr=0x7d;
    upr=0x82;
    dive.min_temperature=divebuf[i-1];
    dive.max_temperature=dive.min_temperature;
    i-=2;
  }
  else {
    dive.datetime=g_strdup_printf("%04d-%02d-%02d %02d:%02d:00",
      divebuf[i-9]+(divebuf[i-9]>89?1900:2000),
      divebuf[i-10],
      divebuf[i-11],
      divebuf[i-12],
      divebuf[i-13]
    );
    if(last_dive_datetime) {
      if(g_utf8_collate(dive.datetime,last_dive_datetime)<0) {
        g_free(dive.datetime);
        return FALSE;
      }
    }
    interval=divebuf[i-3];
    dive.start_pressure=divebuf[i-5]?divebuf[i-5]*2:NULL_PRESSURE;
    dive.O2=divebuf[i-6]?divebuf[i-6]:21.0;
    i-=14;	
  }

  dive.profile=g_array_new(FALSE,FALSE,sizeof(ImportProfile)); 
  profile_segment.time=0;
  profile_segment.depth=0;
  profile_segment.temperature=NULL_TEMPERATURE;	
  g_array_append_val(dive.profile,profile_segment);

  dive.duration=0;
  dive.max_depth=0.0;
  while(divebuf[i]!=0x80) {
    if(divebuf[i]<lwr || divebuf[i]>upr) {
      depth+=convert_feet_to_meters((gchar)divebuf[i]);
      if(depth>dive.max_depth) dive.max_depth=depth;
      dive.duration+=interval;
      profile_segment.time=dive.duration;
      profile_segment.depth=depth;
      profile_segment.temperature=NULL_TEMPERATURE;
      g_array_append_val(dive.profile,profile_segment);
    }
    i--;
  }

  if(lwr==0x7d) { /* is spyder */
    dive.datetime=g_strdup_printf("%04d-%02d-%02d %02d:%02d:00",
      divebuf[i-5]+(divebuf[i-5]>89?1900:2000),
      divebuf[i-4],
      divebuf[i-3],
      divebuf[i-2],
      divebuf[i-1]
    );
    if(g_utf8_collate(dive.datetime,last_dive_datetime)<0) {
      g_free(dive.datetime);
      g_array_free(dive.profile,TRUE);
      return FALSE;
    }
    interval=divebuf[i-8];
    for(j=1;j<dive.profile->len;j++) {
      profile_segmentptr=&g_array_index(dive.profile,ImportProfile,j);
      profile_segmentptr->time=profile_segmentptr->time*interval;
    }
    dive.duration=dive.duration*interval;
  }
  else {
    dive.min_temperature=divebuf[i-1];
    dive.max_temperature=divebuf[i-2];
    if(dive.max_temperature<dive.min_temperature) {
      dive.min_temperature=dive.max_temperature;
      dive.max_temperature=divebuf[i-1];
    }
    dive.end_pressure=divebuf[i-3]?divebuf[i-3]*2:NULL_PRESSURE;
  }	
  g_array_append_val(dive_array,dive);
  return TRUE;
}

static gint suunto_get_dive_tank_id_callback(gint *dive_tank_id,gint argc,gchar **argv, gchar **azColName)
{
  *dive_tank_id=strtol(argv[0],NULL,0);
  return 0;
}

static void suunto_import_dive(SuuntoModel model,SuuntoDive *dive)
{
  gint dive_id,dive_tank_id=0,rc;
  gchar *sql,*SqlErr;

  dive_id=plugin_interface->import_dive(dive->datetime,dive->duration,dive->max_depth,dive->min_temperature,dive->max_temperature,
    NULL_NOTES,NULL_VISIBILITY,NULL_WEIGHT,NULL,NULL,NULL,NULL,NULL,dive->profile,TRUE);	
  if(!IS_SPYDER(model)) {
    sql=g_strdup_printf("SELECT dive_tank_id FROM Dive_Tank WHERE dive_id=%d LIMIT 1",dive_id);
    rc=plugin_interface->sql_query(sql,suunto_get_dive_tank_id_callback,&dive_tank_id,&SqlErr);
    if(rc==SQLITE_OK) {
      g_free(sql);
      if(dive_tank_id)
        sql=g_strdup_printf("UPDATE Dive_Tank SET dive_tank_O2=%f,dive_tank_spressure=%f,dive_tank_epressure=%f WHERE dive_tank_id=%d",dive->O2,dive->start_pressure,dive->end_pressure,dive_tank_id);
      else
        sql=g_strdup_printf("INSERT INTO Dive_Tank (dive_id,tank_id,dive_tank_avg_depth,dive_tank_O2,dive_tank_He,dive_tank_stime,dive_tank_etime,dive_tank_spressure,dive_tank_epressure) VALUES (%d,0,%f,%f,%f,%d,%d,%f,%f)",
          dive_id,NULL_DEPTH,dive->O2,NULL_PERCENT,NULL_DURATION,NULL_DURATION,dive->start_pressure,dive->end_pressure
        );
      rc=plugin_interface->sql_query(sql,NULL,0,&SqlErr);
    }
    else sqlite3_free(SqlErr);
    if(rc!=SQLITE_OK) {
      g_printerr(_("Unable to save tank_data.\n\nErrMsg=\'%s\'\n\nrc=%d."),SqlErr,rc);
      sqlite3_free(SqlErr);
    }
    g_free(sql);
  }
}

static gint suunto_sort_func(GtkTreeModel *model,GtkTreeIter  *a,GtkTreeIter  *b,gint  col_id)
{
  gint rval=0;
  SuuntoDive *dive1,*dive2;
	
  gtk_tree_model_get(GTK_TREE_MODEL(model),a,SUUNTO_COL_DATA,&dive1,-1);
  gtk_tree_model_get(GTK_TREE_MODEL(model),b,SUUNTO_COL_DATA,&dive2,-1);
  switch(col_id) {
    case SUUNTO_COL_DATE     : rval=g_utf8_collate(dive1->datetime,dive2->datetime);
                               break;
    case SUUNTO_COL_MAXDEPTH : if(dive1->max_depth<dive2->max_depth) rval=-1;
                               else if(dive1->max_depth>dive2->max_depth) rval=1;
                               break;
    case SUUNTO_COL_DURATION : rval=dive1->duration-dive2->duration;
                               break;
    case SUUNTO_COL_MAXTEMP  : if(dive1->max_temperature<dive2->max_temperature) rval=-1;
                               else if(dive1->max_temperature>dive2->max_temperature) rval=1;
                               break;
    case SUUNTO_COL_MINTEMP  : if(dive1->min_temperature<dive2->min_temperature) rval=-1;
                               else if(dive1->min_temperature>dive2->min_temperature) rval=1;
  }
  return rval;
}

static void suunto_free_dive_array(GArray *dive_array)
{
  gint i;
  SuuntoDive *dive;
  
  for(i=0;i<dive_array->len;i++) {
    dive=&g_array_index(dive_array,SuuntoDive,i);
    g_array_free(dive->profile,TRUE);
  }
  g_array_free(dive_array,TRUE);
}

static void on_suunto_select_all_btn_clicked(GtkButton *button,SuuntoSelectDivesWindowData *user_data)
{
  gtk_tree_selection_select_all(gtk_tree_view_get_selection(GTK_TREE_VIEW(user_data->dive_list)));
  gtk_widget_set_sensitive(user_data->import_btn,TRUE);
}

static void on_suunto_unselect_all_btn_clicked(GtkButton *button,SuuntoSelectDivesWindowData *user_data)
{
  gtk_tree_selection_unselect_all(gtk_tree_view_get_selection(GTK_TREE_VIEW(user_data->dive_list)));
  gtk_widget_set_sensitive(user_data->import_btn,FALSE);
}

static void suunto_select_dives_cleanup(SuuntoSelectDivesWindowData *suunto_select_dives_window_data)
{
  suunto_free_dive_array(suunto_select_dives_window_data->dives_array);
  g_free(suunto_select_dives_window_data);
}

static void on_suunto_select_dives_import_btn_clicked(GtkButton *button,SuuntoSelectDivesWindowData *user_data)
{
  GtkTreeSelection *selection;
  GtkTreeIter iter;
  GList *selected_dives,*current_dive;
  gint num_dives,current_dive_num=1;
  gdouble progress_step,progress_value=0;
  gchar *progress_text;
  SuuntoDive *dive;

  gtk_widget_set_sensitive(user_data->import_btn,FALSE);
  gtk_widget_set_sensitive(user_data->select_all_btn,FALSE);
  gtk_widget_set_sensitive(user_data->unselect_all_btn,FALSE);

  selection=gtk_tree_view_get_selection(GTK_TREE_VIEW(user_data->dive_list));
  selected_dives=gtk_tree_selection_get_selected_rows(selection,&user_data->dive_list_model);

  num_dives=gtk_tree_selection_count_selected_rows(selection);
  progress_step=1.0/num_dives;

  user_data->cancel_import=FALSE;
  current_dive=g_list_first(selected_dives);
  plugin_interface->begin_transaction();
  user_data->import_in_progess=TRUE;
  while(current_dive) {
    if(user_data->cancel_import) {
      plugin_interface->rollback_transaction();
      break;
    }
    else {
      gtk_tree_model_get_iter(user_data->dive_list_model,&iter,(GtkTreePath*)current_dive->data);
      gtk_tree_model_get(user_data->dive_list_model,&iter,
        SUUNTO_COL_DATA,&dive,
        -1
      );
      suunto_import_dive(user_data->model,dive);
    }

    progress_value+=progress_step;
    if(progress_value>1.0) progress_value=1.0;
    progress_text=g_strdup_printf("%d/%d dives imported",current_dive_num++,num_dives);
    gtk_progress_bar_set_text(GTK_PROGRESS_BAR(user_data->progressbar),progress_text);
    g_free(progress_text);
    gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(user_data->progressbar),progress_value);
    while(gtk_events_pending()) gtk_main_iteration();
    current_dive=g_list_next(current_dive);
  }
  if(!user_data->cancel_import) {
    plugin_interface->commit_transaction();
    plugin_interface->refresh_dive_list(0);
  }
  g_list_foreach(selected_dives,(GFunc)gtk_tree_path_free,NULL);
  g_list_free(selected_dives);
  gtk_widget_destroy(user_data->window);
  suunto_select_dives_cleanup(user_data);
}

static void on_suunto_cancel_import_btn_clicked(GtkButton *button,SuuntoSelectDivesWindowData *user_data)
{
  if(user_data->import_in_progess) user_data->cancel_import=TRUE;
  else {
    gtk_widget_destroy(user_data->window);
    suunto_select_dives_cleanup(user_data);
  }
}

static void on_suunto_select_dives_list_cursor_changed(GtkTreeView *treeview,SuuntoSelectDivesWindowData *user_data)
{
  if(gtk_tree_selection_count_selected_rows(gtk_tree_view_get_selection(treeview))) gtk_widget_set_sensitive(user_data->import_btn,TRUE);
}

static gboolean on_suunto_select_dives_window_delete(GtkWidget *widget,GdkEvent *event,SuuntoSelectDivesWindowData *user_data)
{
  on_suunto_cancel_import_btn_clicked(NULL,user_data);
  return TRUE;
}

static void suunto_create_select_dives_window(SuuntoModel model,GArray *dives_array)
{

  GtkWidget *alignment_select_dives;
  GtkWidget *vbox_select_dives;
  GtkWidget *scrolledwindow_diveslist;
  GtkWidget *hbuttonbox_select_dives;
  GtkWidget *alignment1;
  GtkWidget *hbox1;
  GtkWidget *image1;
  GtkWidget *label1;
  GtkWidget *suunto_cancel_import_btn;
  GtkTreeViewColumn *column;
  SuuntoSelectDivesWindowData *suunto_select_dives_window_data;
  SuuntoDive *dive;
  gint i;
  gchar *datestr,*timestr,*depthstr,*durationstr,*maxtempstr,*mintempstr,buf[30];
  struct tm t;
  GtkTreeIter iter;
	
  suunto_select_dives_window_data=g_malloc(sizeof(SuuntoSelectDivesWindowData));
  g_return_if_fail(suunto_select_dives_window_data);

  suunto_select_dives_window_data->model=model;
  suunto_select_dives_window_data->dives_array=dives_array;
  suunto_select_dives_window_data->import_in_progess=FALSE;

  suunto_select_dives_window_data->window=gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(suunto_select_dives_window_data->window),_("Select Dives"));  
  gtk_window_set_default_size(GTK_WINDOW(suunto_select_dives_window_data->window),520,300);	
  gtk_window_set_position(GTK_WINDOW(suunto_select_dives_window_data->window),GTK_WIN_POS_CENTER_ON_PARENT);
  gtk_window_set_type_hint(GTK_WINDOW(suunto_select_dives_window_data->window),GDK_WINDOW_TYPE_HINT_DIALOG);

  alignment_select_dives=gtk_alignment_new(0.5,0.5,1,1);
  gtk_widget_show(alignment_select_dives);
  gtk_container_add(GTK_CONTAINER(suunto_select_dives_window_data->window),alignment_select_dives);
  gtk_alignment_set_padding(GTK_ALIGNMENT(alignment_select_dives),4,4,4,4);

  vbox_select_dives=gtk_vbox_new(FALSE, 4);
  gtk_widget_show(vbox_select_dives);
  gtk_container_add(GTK_CONTAINER(alignment_select_dives),vbox_select_dives);

  scrolledwindow_diveslist=gtk_scrolled_window_new(NULL, NULL);
  gtk_widget_show(scrolledwindow_diveslist);
  gtk_box_pack_start(GTK_BOX (vbox_select_dives),scrolledwindow_diveslist,TRUE,TRUE,0);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwindow_diveslist),GTK_POLICY_AUTOMATIC,GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolledwindow_diveslist),GTK_SHADOW_IN);

  suunto_select_dives_window_data->dive_list=gtk_tree_view_new();
  gtk_widget_show(suunto_select_dives_window_data->dive_list);
  gtk_container_add(GTK_CONTAINER(scrolledwindow_diveslist),suunto_select_dives_window_data->dive_list);

  suunto_select_dives_window_data->progressbar=gtk_progress_bar_new();
  gtk_widget_show(suunto_select_dives_window_data->progressbar);
  gtk_box_pack_start(GTK_BOX(vbox_select_dives),suunto_select_dives_window_data->progressbar,FALSE,FALSE,0);
  
  hbuttonbox_select_dives=gtk_hbutton_box_new();
  gtk_widget_show(hbuttonbox_select_dives);
  gtk_box_pack_start(GTK_BOX(vbox_select_dives),hbuttonbox_select_dives,FALSE,FALSE,0);
  gtk_button_box_set_layout(GTK_BUTTON_BOX(hbuttonbox_select_dives),GTK_BUTTONBOX_SPREAD);

  suunto_select_dives_window_data->import_btn=gtk_button_new();
  gtk_widget_show(suunto_select_dives_window_data->import_btn);
  gtk_container_add(GTK_CONTAINER(hbuttonbox_select_dives),suunto_select_dives_window_data->import_btn);
  GTK_WIDGET_SET_FLAGS(suunto_select_dives_window_data->import_btn, GTK_CAN_DEFAULT);
  gtk_widget_set_sensitive(GTK_WIDGET(suunto_select_dives_window_data->import_btn),FALSE);

  alignment1=gtk_alignment_new(0.5,0.5,0,0);
  gtk_widget_show(alignment1);
  gtk_container_add(GTK_CONTAINER(suunto_select_dives_window_data->import_btn),alignment1);

  hbox1=gtk_hbox_new(FALSE, 2);
  gtk_widget_show(hbox1);
  gtk_container_add(GTK_CONTAINER(alignment1),hbox1);
  
  suunto_select_dives_window_data->select_all_btn=gtk_button_new_with_mnemonic(_("_Select All"));
  gtk_widget_show(suunto_select_dives_window_data->select_all_btn);
  gtk_container_add(GTK_CONTAINER(hbuttonbox_select_dives),suunto_select_dives_window_data->select_all_btn);
  GTK_WIDGET_SET_FLAGS(suunto_select_dives_window_data->select_all_btn,GTK_CAN_DEFAULT);

  suunto_select_dives_window_data->unselect_all_btn=gtk_button_new_with_mnemonic(_("_Unselect All"));
  gtk_widget_show(suunto_select_dives_window_data->unselect_all_btn);
  gtk_container_add(GTK_CONTAINER(hbuttonbox_select_dives),suunto_select_dives_window_data->unselect_all_btn);
  GTK_WIDGET_SET_FLAGS(suunto_select_dives_window_data->unselect_all_btn,GTK_CAN_DEFAULT);

  image1=gtk_image_new_from_stock("gtk-apply",GTK_ICON_SIZE_BUTTON);
  gtk_widget_show(image1);
  gtk_box_pack_start(GTK_BOX(hbox1),image1,FALSE,FALSE,0);

  label1=gtk_label_new_with_mnemonic(_("_Import"));
  gtk_widget_show (label1);
  gtk_box_pack_start(GTK_BOX(hbox1),label1,FALSE,FALSE,0);

  suunto_cancel_import_btn=gtk_button_new_from_stock("gtk-cancel");
  gtk_widget_show(suunto_cancel_import_btn);
  gtk_container_add(GTK_CONTAINER(hbuttonbox_select_dives),suunto_cancel_import_btn);
  GTK_WIDGET_SET_FLAGS(suunto_cancel_import_btn,GTK_CAN_DEFAULT);
  
  g_signal_connect((gpointer)suunto_select_dives_window_data->select_all_btn,"clicked",G_CALLBACK(on_suunto_select_all_btn_clicked),suunto_select_dives_window_data);
  g_signal_connect((gpointer)suunto_select_dives_window_data->unselect_all_btn,"clicked",G_CALLBACK(on_suunto_unselect_all_btn_clicked),suunto_select_dives_window_data);
  g_signal_connect((gpointer)suunto_select_dives_window_data->import_btn,"clicked",G_CALLBACK(on_suunto_select_dives_import_btn_clicked),suunto_select_dives_window_data); 
  g_signal_connect((gpointer)suunto_cancel_import_btn,"clicked",G_CALLBACK(on_suunto_cancel_import_btn_clicked),suunto_select_dives_window_data); 
  g_signal_connect((gpointer)suunto_select_dives_window_data->window,"delete-event",G_CALLBACK(on_suunto_select_dives_window_delete),suunto_select_dives_window_data);

  suunto_select_dives_window_data->dive_list_model=GTK_TREE_MODEL(gtk_list_store_new(
    SUUNTO_NUM_COLS,
    G_TYPE_STRING,
    G_TYPE_STRING,
    G_TYPE_STRING,
    G_TYPE_STRING,
    G_TYPE_STRING,
    G_TYPE_STRING,
    G_TYPE_BOOLEAN,
    G_TYPE_POINTER
  ));
  
  column=gtk_tree_view_column_new_with_attributes(_("Date"),gtk_cell_renderer_text_new(),"text",SUUNTO_COL_DATE,NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(suunto_select_dives_window_data->dive_list),column);
  gtk_tree_view_column_set_sort_column_id(column,SUUNTO_COL_DATE);
  gtk_tree_sortable_set_sort_func(GTK_TREE_SORTABLE(suunto_select_dives_window_data->dive_list_model),SUUNTO_COL_DATE,(GtkTreeIterCompareFunc)suunto_sort_func,(gpointer)SUUNTO_COL_DATE,NULL);

  column=gtk_tree_view_column_new_with_attributes(_("Time"),gtk_cell_renderer_text_new(),"text",SUUNTO_COL_TIME,NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(suunto_select_dives_window_data->dive_list),column);
  gtk_tree_view_column_set_sort_column_id(column,SUUNTO_COL_TIME);
  
  column=gtk_tree_view_column_new_with_attributes(_("Max Depth"),gtk_cell_renderer_text_new(),"text",SUUNTO_COL_MAXDEPTH,NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(suunto_select_dives_window_data->dive_list),column);
  gtk_tree_view_column_set_sort_column_id(column,SUUNTO_COL_MAXDEPTH);
  gtk_tree_sortable_set_sort_func(GTK_TREE_SORTABLE(suunto_select_dives_window_data->dive_list_model),SUUNTO_COL_MAXDEPTH,(GtkTreeIterCompareFunc)suunto_sort_func,(gpointer)SUUNTO_COL_MAXDEPTH,NULL);  

  column=gtk_tree_view_column_new_with_attributes(_("Duration"),gtk_cell_renderer_text_new(),"text",SUUNTO_COL_DURATION,NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(suunto_select_dives_window_data->dive_list),column);
  gtk_tree_view_column_set_sort_column_id(column,SUUNTO_COL_DURATION);
  gtk_tree_sortable_set_sort_func(GTK_TREE_SORTABLE(suunto_select_dives_window_data->dive_list_model),SUUNTO_COL_DURATION,(GtkTreeIterCompareFunc)suunto_sort_func,(gpointer)SUUNTO_COL_DURATION,NULL);
  
  column=gtk_tree_view_column_new_with_attributes(_("Max Temp"),gtk_cell_renderer_text_new(),"text",SUUNTO_COL_MAXTEMP,NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(suunto_select_dives_window_data->dive_list),column);
  gtk_tree_view_column_set_sort_column_id(column,SUUNTO_COL_MAXTEMP);
  gtk_tree_sortable_set_sort_func(GTK_TREE_SORTABLE(suunto_select_dives_window_data->dive_list_model),SUUNTO_COL_MAXTEMP,(GtkTreeIterCompareFunc)suunto_sort_func,(gpointer)SUUNTO_COL_MAXTEMP,NULL);
  
  column=gtk_tree_view_column_new_with_attributes(_("Min Temp"),gtk_cell_renderer_text_new(),"text",SUUNTO_COL_MINTEMP,NULL);
  gtk_tree_view_append_column(GTK_TREE_VIEW(suunto_select_dives_window_data->dive_list),column);
  gtk_tree_view_column_set_sort_column_id(column,SUUNTO_COL_MINTEMP);
  gtk_tree_sortable_set_sort_func(GTK_TREE_SORTABLE(suunto_select_dives_window_data->dive_list_model),SUUNTO_COL_MAXTEMP,(GtkTreeIterCompareFunc)suunto_sort_func,(gpointer)SUUNTO_COL_MINTEMP,NULL);
  
  gtk_tree_view_set_model(GTK_TREE_VIEW(suunto_select_dives_window_data->dive_list),suunto_select_dives_window_data->dive_list_model);
  g_object_unref(suunto_select_dives_window_data->dive_list_model);
  gtk_tree_selection_set_mode(GTK_TREE_SELECTION(gtk_tree_view_get_selection(GTK_TREE_VIEW(suunto_select_dives_window_data->dive_list))),GTK_SELECTION_MULTIPLE);

  for(i=0;i<dives_array->len;i++) {
    dive=&g_array_index(dives_array,SuuntoDive,i);
    strptime(dive->datetime,DATETIME_STR_FMT,&t);
    strftime(buf,30,"%x",&t);
    datestr=g_strdup(buf);
    strftime(buf,30,"%X",&t);
    timestr=g_strdup(buf);
    depthstr=plugin_interface->format_field_depth(plugin_interface->is_depth_metric()?dive->max_depth:convert_meters_to_feet(dive->max_depth));
    durationstr=plugin_interface->format_field_time(dive->duration);
    maxtempstr=plugin_interface->format_field_temperature(plugin_interface->is_temperature_metric()?dive->max_temperature:convert_celsius_to_farenheit(dive->max_temperature));
    mintempstr=plugin_interface->format_field_temperature(plugin_interface->is_temperature_metric()?dive->min_temperature:convert_celsius_to_farenheit(dive->min_temperature));
    gtk_list_store_append(GTK_LIST_STORE(suunto_select_dives_window_data->dive_list_model),&iter);
    gtk_list_store_set(GTK_LIST_STORE(suunto_select_dives_window_data->dive_list_model),&iter,
      SUUNTO_COL_DATE,datestr,
      SUUNTO_COL_TIME,timestr,
      SUUNTO_COL_MAXDEPTH,depthstr,
      SUUNTO_COL_DURATION,durationstr,
      SUUNTO_COL_MAXTEMP,maxtempstr,
      SUUNTO_COL_MINTEMP,mintempstr,
      SUUNTO_COL_DATA,(gpointer)dive,
      -1
    );
    g_free(datestr);
    g_free(timestr);
    g_free(durationstr);
    g_free(depthstr);
    g_free(maxtempstr);
    g_free(mintempstr);
  }
  g_signal_connect((gpointer)suunto_select_dives_window_data->dive_list,"cursor-changed",G_CALLBACK(on_suunto_select_dives_list_cursor_changed),suunto_select_dives_window_data);					

  gtk_window_set_transient_for(GTK_WINDOW(suunto_select_dives_window_data->window),GTK_WINDOW(plugin_interface->main_window));  
  gtk_widget_show(suunto_select_dives_window_data->window);
}

static void on_suunto_download_ok_btn_clicked(GtkButton *button,SuuntoDownloadWindowData *user_data)
{
  gint fd,len,count=1,pb_count=1;
  gchar *msg,*last_dive_datetime=NULL,*device;
  guchar divebuf[16*1024];
  SuuntoModel model;
  SuuntoDive *dive;
  GArray *dive_array;

  gtk_widget_set_sensitive(user_data->ok_btn,FALSE);
  while(gtk_events_pending()) gtk_main_iteration(); 
  device=gtk_combo_box_get_active_text(GTK_COMBO_BOX(user_data->device));
  fd=suunto_open(device);
  if(fd>=0) {
    user_data->download_in_progress=TRUE;
    user_data->cancel_download=FALSE;
    if(suunto_detect_interface(fd)) {
      model=suunto_get_model(fd);
      	
      if(model!=SUUNTO_MODEL_UNKNOWN) {
        if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(user_data->new_dives)))
          last_dive_datetime=plugin_interface->get_last_dive_datetime();
        dive_array=g_array_new(FALSE,FALSE,sizeof(SuuntoDive));
        len=suunto_get_dive(fd,SUUNTO_DIVE_FIRST,divebuf,sizeof(divebuf));
        while(len>14 && !user_data->cancel_download) {	
          if(suunto_parse_dive(divebuf,len,model,last_dive_datetime,dive_array)) {
            msg=g_strdup_printf(_("%s: %d %s read"),suunto_model_names[model],count,count==1?_("dive"):_("dives"));
            if(pb_count>10) pb_count=1;
            gtk_progress_bar_set_fraction(progressbar,(gdouble)pb_count/10);
			gtk_progress_bar_set_text(progressbar,msg);
			while(gtk_events_pending()) gtk_main_iteration();
            g_free(msg);
            pb_count++;
            count++;

            len=suunto_get_dive(fd,SUUNTO_DIVE_NEXT,divebuf,sizeof(divebuf));
          }
          else len=0;
        }
        if(!user_data->cancel_download) {
          if(!gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(user_data->select_dives))) {
            for(count=0;count<dive_array->len;count++) {
              dive=&g_array_index(dive_array,SuuntoDive,count);
              suunto_import_dive(model,dive);
            }
            suunto_free_dive_array(dive_array);
            plugin_interface->refresh_dive_list(0);
          }
          else suunto_create_select_dives_window(model,dive_array);
          gtk_widget_destroy(user_data->window);
        }
        else {
          suunto_free_dive_array(dive_array);
          if(user_data->download_in_progress) gtk_widget_destroy(user_data->window);
        }
      }
      else gtk_widget_set_sensitive(user_data->ok_btn,TRUE);
      user_data->download_in_progress=FALSE;
    }
    suunto_close(fd);
  }
  else if(fd==NOT_SERIAL_PORT) g_printerr(_("%s is not a serial port."),device);
  else g_printerr(_("%s is an invalid device"),device);
  gtk_widget_set_sensitive(user_data->ok_btn,TRUE);
}

static void on_suunto_download_cancel_btn_clicked(GtkButton *button,SuuntoDownloadWindowData *user_data)
{	
  if(user_data->download_in_progress) user_data->cancel_download=TRUE;
  else {
    gtk_widget_destroy(user_data->window);
    g_free(user_data);
  }
}

static gboolean on_suunto_download_window_delete(GtkWidget *widget,GdkEvent *event,SuuntoDownloadWindowData *user_data)
{
  on_suunto_download_cancel_btn_clicked(NULL,user_data);
  return TRUE;
}

void suunto_show_download_window(void)
{

  GtkWidget *alignment_suunto_download_window;
  GtkWidget *vbox_suunto_download_window;
  GtkWidget *hbox_suunto_device;
  GtkWidget *label3;
  GtkWidget *frame_select_download_type;
  GtkWidget *alignment_select_download_type;
  GtkWidget *vbox_select_download_type;
  GSList *new_dives_group = NULL;
  GtkWidget *label_frame_select_download_type;
  GtkWidget *hbuttonbox_suunto_download_window;
  GtkWidget *suunto_cancel_btn;
  SuuntoDownloadWindowData *suunto_download_window_data;

  suunto_download_window_data=g_malloc(sizeof(SuuntoDownloadWindowData));
  g_return_if_fail(suunto_download_window_data);
  suunto_download_window_data->cancel_download=FALSE;
  suunto_download_window_data->download_in_progress=FALSE;

  suunto_download_window_data->window=gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(suunto_download_window_data->window),_("Download Suunto Dive Computer"));
  gtk_window_set_position(GTK_WINDOW(suunto_download_window_data->window),GTK_WIN_POS_CENTER_ON_PARENT);
  gtk_window_set_modal(GTK_WINDOW(suunto_download_window_data->window),TRUE);
  gtk_window_set_resizable(GTK_WINDOW(suunto_download_window_data->window),FALSE);
  gtk_window_set_type_hint(GTK_WINDOW(suunto_download_window_data->window),GDK_WINDOW_TYPE_HINT_DIALOG);
  
  alignment_suunto_download_window=gtk_alignment_new(0.5,0.5,1,1);
  gtk_widget_show(alignment_suunto_download_window);
  gtk_container_add(GTK_CONTAINER(suunto_download_window_data->window),alignment_suunto_download_window);
  gtk_alignment_set_padding(GTK_ALIGNMENT(alignment_suunto_download_window),4,4,4,4);

  vbox_suunto_download_window=gtk_vbox_new(FALSE, 4);
  gtk_widget_show(vbox_suunto_download_window);
  gtk_container_add(GTK_CONTAINER(alignment_suunto_download_window),vbox_suunto_download_window);

  hbox_suunto_device=gtk_hbox_new (FALSE, 4);
  gtk_widget_show(hbox_suunto_device);
  gtk_box_pack_start(GTK_BOX(vbox_suunto_download_window),hbox_suunto_device,FALSE,TRUE,0);

  label3 = gtk_label_new(_("Suunto Device:"));
  gtk_widget_show(label3);
  gtk_box_pack_start(GTK_BOX(hbox_suunto_device),label3,FALSE,FALSE,0);

  suunto_download_window_data->device=gtk_combo_box_entry_new_text();
  gtk_widget_show(suunto_download_window_data->device);
  gtk_box_pack_start(GTK_BOX(hbox_suunto_device),suunto_download_window_data->device,TRUE,TRUE,0);

  frame_select_download_type=gtk_frame_new(NULL);
  gtk_widget_show(frame_select_download_type);
  gtk_box_pack_start(GTK_BOX(vbox_suunto_download_window),frame_select_download_type,TRUE,TRUE,0);
  gtk_container_set_border_width(GTK_CONTAINER(frame_select_download_type),1);
  gtk_frame_set_shadow_type(GTK_FRAME(frame_select_download_type),GTK_SHADOW_OUT);

  alignment_select_download_type=gtk_alignment_new(0.5,0.5,1,1);
  gtk_widget_show(alignment_select_download_type);
  gtk_container_add(GTK_CONTAINER(frame_select_download_type),alignment_select_download_type);
  gtk_alignment_set_padding(GTK_ALIGNMENT(alignment_select_download_type),0,0,12,0);

  vbox_select_download_type=gtk_vbox_new(FALSE,4);
  gtk_widget_show(vbox_select_download_type);
  gtk_container_add(GTK_CONTAINER(alignment_select_download_type),vbox_select_download_type);

  suunto_download_window_data->new_dives=gtk_radio_button_new_with_mnemonic(NULL,_("_New Dives"));
  gtk_widget_show(suunto_download_window_data->new_dives);
  gtk_box_pack_start(GTK_BOX(vbox_select_download_type),suunto_download_window_data->new_dives,FALSE,FALSE,0);
  gtk_radio_button_set_group(GTK_RADIO_BUTTON(suunto_download_window_data->new_dives),new_dives_group);
  new_dives_group=gtk_radio_button_get_group(GTK_RADIO_BUTTON(suunto_download_window_data->new_dives));

  suunto_download_window_data->all_dives=gtk_radio_button_new_with_mnemonic(NULL,_("_All Dives"));
  gtk_widget_show(suunto_download_window_data->all_dives);
  gtk_box_pack_start(GTK_BOX (vbox_select_download_type),suunto_download_window_data->all_dives,FALSE,FALSE,0);
  gtk_radio_button_set_group(GTK_RADIO_BUTTON(suunto_download_window_data->all_dives), new_dives_group);
  new_dives_group=gtk_radio_button_get_group(GTK_RADIO_BUTTON(suunto_download_window_data->all_dives));

  suunto_download_window_data->select_dives=gtk_check_button_new_with_label(_("Select Dives"));
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(suunto_download_window_data->select_dives),FALSE);
  gtk_widget_show(suunto_download_window_data->select_dives);
  gtk_box_pack_start(GTK_BOX(vbox_select_download_type),suunto_download_window_data->select_dives,FALSE,FALSE,0);

  label_frame_select_download_type=gtk_label_new(_("<b>Download</b>"));
  gtk_widget_show(label_frame_select_download_type);
  gtk_frame_set_label_widget(GTK_FRAME(frame_select_download_type),label_frame_select_download_type);
  gtk_label_set_use_markup(GTK_LABEL(label_frame_select_download_type),TRUE);
  
  progressbar=GTK_PROGRESS_BAR(gtk_progress_bar_new());
  gtk_widget_show(GTK_WIDGET(progressbar));
  gtk_container_add(GTK_CONTAINER(vbox_suunto_download_window),GTK_WIDGET(progressbar));

  hbuttonbox_suunto_download_window=gtk_hbutton_box_new();
  gtk_widget_show(hbuttonbox_suunto_download_window);
  gtk_box_pack_start(GTK_BOX(vbox_suunto_download_window),hbuttonbox_suunto_download_window,FALSE,FALSE,0);
  gtk_button_box_set_layout(GTK_BUTTON_BOX(hbuttonbox_suunto_download_window),GTK_BUTTONBOX_SPREAD);

  suunto_download_window_data->ok_btn=gtk_button_new_from_stock("gtk-ok");
  gtk_widget_show(suunto_download_window_data->ok_btn);
  gtk_container_add(GTK_CONTAINER(hbuttonbox_suunto_download_window),suunto_download_window_data->ok_btn);
  GTK_WIDGET_SET_FLAGS(suunto_download_window_data->ok_btn,GTK_CAN_DEFAULT);

  suunto_cancel_btn=gtk_button_new_from_stock("gtk-cancel");
  gtk_widget_show(suunto_cancel_btn);
  gtk_container_add(GTK_CONTAINER(hbuttonbox_suunto_download_window),suunto_cancel_btn);
  GTK_WIDGET_SET_FLAGS(suunto_cancel_btn, GTK_CAN_DEFAULT);
  
  g_signal_connect((gpointer)suunto_download_window_data->ok_btn,"clicked",G_CALLBACK(on_suunto_download_ok_btn_clicked),suunto_download_window_data);
  g_signal_connect((gpointer)suunto_cancel_btn, "clicked",G_CALLBACK(on_suunto_download_cancel_btn_clicked),suunto_download_window_data);
  g_signal_connect((gpointer)suunto_download_window_data->window,"delete-event",G_CALLBACK(on_suunto_download_window_delete),suunto_download_window_data);
  
  suunto_load_devices_liststore(suunto_download_window_data->device_list_store,GTK_COMBO_BOX(suunto_download_window_data->device));

  gtk_window_set_transient_for(GTK_WINDOW(suunto_download_window_data->window),GTK_WINDOW(plugin_interface->main_window));
  gtk_widget_show(suunto_download_window_data->window);
}

void suunto_download_plugin(void)
{
  suunto_show_download_window();
}

void gdivelog_plugin(GDiveLogPluginInterface *gdivelog_plugin_interface)
{
  plugin_interface=gdivelog_plugin_interface;
  plugin_interface->plugin_register("suunto_download_plugin",PLUGIN_TYPE_DOWNLOAD,"Download Suunto dive computer from serial port");
}
