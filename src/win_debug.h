#ifndef _WIN_DEBUG_H_
#define _WIN_DEBUG_H_
#include <stdio.h>

/* Send log messages over UDP from a running VST plugin.
   On the (Linux) development host, do this:

   socat UDP-RECV:12345 -
*/

#ifndef LOG_IP
#define LOG_IP "192.168.6.8"
#endif

#ifndef LOG_PORT
#define LOG_PORT 12345
#endif

// http://stackoverflow.com/questions/679145/how-to-set-up-a-winsock-udp-socket
void log_send(const char *msg, ...) {

    static int open;
    static char buf[1024];
    static sockaddr_in dest;
    static sockaddr_in local;
    static WSAData data;
    static SOCKET s;

    if (!open) {
        open = 1;
        WSAStartup( MAKEWORD( 2, 2 ), &data );

        local.sin_family = AF_INET;
        local.sin_addr.s_addr = inet_addr( "0.0.0.0" );
        local.sin_port = 0; // choose any

        dest.sin_family = AF_INET;
        dest.sin_addr.s_addr = inet_addr( LOG_IP );
        dest.sin_port = htons( LOG_PORT );

        s = socket( AF_INET, SOCK_DGRAM, IPPROTO_UDP );
        bind( s, (sockaddr *)&local, sizeof(local) );
    }
    va_list ap;
    va_start(ap, msg);
    vsprintf(buf, msg, ap);
    va_end(ap);
    int ret = sendto( s, buf, strlen(buf), 0, (sockaddr *)&dest, sizeof(dest) );
}
#ifndef LOG
#define LOG log_send
// #define LOG(...) fprintf(stderr, __VA_ARGS__)
#endif
#endif // _WIN_DEBUG_H_
