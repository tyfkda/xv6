#include "stdio.h"
#include "stdlib.h"
#include "unistd.h"

int main(void) {
  int MAC_SIZE = 18;
  //const char* ip = "192.168.2.1";
  const char* ip = "172.17.0.1";
  //const char* ip = "10.0.2.2";
  char* mac = malloc(MAC_SIZE);
  if(arp("mynet0", ip, mac, MAC_SIZE) < 0) {
    printf("ARP for IP:%s Failed.\n", ip);
    return 1;
  }
  printf("Result: [%s]\n", mac);
  return 0;
}
