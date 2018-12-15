#include "stdio.h"
#include "stdlib.h"
#include "unistd.h"

int main(int argc, char** argv) {
  const int MAC_SIZE = 18;

  const char* ip = "172.17.0.1";
  if (argc >= 2) {
    ip = argv[1];
  }

  char* mac = malloc(MAC_SIZE);
  if(arp("mynet0", ip, mac, MAC_SIZE) < 0) {
    printf("ARP for IP:%s Failed.\n", ip);
    return 1;
  }
  printf("IP:[%s], MAC[%s]\n", ip, mac);
  return 0;
}
