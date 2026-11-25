/* Frozen legacy-format fixture writer. Input: database TAB hex-key TAB JSON. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lmdb.h"
static void ok(int c){if(c){fprintf(stderr,"%s\n",mdb_strerror(c));exit(1);}}
int main(int argc,char **argv){
 MDB_env *e;MDB_txn*t;MDB_dbi d;
 ok(mdb_env_create(&e));ok(mdb_env_set_maxdbs(e,8));ok(mdb_env_set_mapsize(e,64*1024*1024));
 ok(mdb_env_open(e,argv[1],MDB_NOSUBDIR|MDB_NOTLS,0600));ok(mdb_txn_begin(e,NULL,0,&t));
 char *names[]={"state","receipts","expiry","audit","metadata"};
 for(int i=0;i<5;i++)ok(mdb_dbi_open(t,names[i],MDB_CREATE,&d));
 char *line=NULL;size_t cap=0;ssize_t n;
 while((n=getline(&line,&cap,stdin))>0){
  if(line[n-1]=='\n')line[--n]=0;
  char *hex=strchr(line,'\t');if(!hex)return 2;*hex++=0;
  char *value=strchr(hex,'\t');if(!value)return 2;*value++=0;
  size_t len=strlen(hex)/2;unsigned char *key=malloc(len);
  for(size_t i=0;i<len;i++){unsigned int b;if(sscanf(hex+i*2,"%2x",&b)!=1)return 2;key[i]=b;}
  ok(mdb_dbi_open(t,line,0,&d));MDB_val k={len,key},v={strlen(value),value};ok(mdb_put(t,d,&k,&v,0));free(key);
 }
 free(line);ok(mdb_txn_commit(t));mdb_env_close(e);return 0;
}
