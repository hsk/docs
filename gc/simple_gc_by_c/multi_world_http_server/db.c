#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mysql/mysql.h>
int main(void) {
  MYSQL *conn     = NULL;
  MYSQL_RES *resp = NULL;
  MYSQL_ROW row;
  char sql_str[255];
  char *sql_serv  = "localhost";
  char *user      = "root";
  char *passwd    = "";
  char *db_name   = "db_test";

  memset(&sql_str[0], 0x00, sizeof(sql_str));

  // mysql接続
  conn = mysql_init(NULL);
  if(!mysql_real_connect(conn,sql_serv,user,passwd,db_name,0,NULL,0)){
    // error
    exit(-1);
  }

  // クエリ実行
  snprintf(&sql_str[0], sizeof(sql_str)-1, "select * from tb_test");
  if (mysql_query(conn, &sql_str[0])){
    // error
    mysql_close(conn);
    exit(-1);
  }

  // レスポンス
  resp = mysql_use_result(conn);
  while((row = mysql_fetch_row(resp)) != NULL) {
    printf( "%d : %s\n", atoi(row[0]), row[1] );
  }

  // 後片づけ
  mysql_free_result(resp);
  mysql_close(conn);
  return 0;
}
