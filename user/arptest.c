#include "stdio.h"
#include "stdlib.h"
#include "unistd.h"

int main(void) {
  int MAC_SIZE = 18;
  char* ip = "192.168.2.1";
  char* mac = malloc(MAC_SIZE);
  if(arp(ip, mac, MAC_SIZE) < 0) {
    printf("ARP for IP:%s Failed.\n", ip);
  }
  return 0;
}
