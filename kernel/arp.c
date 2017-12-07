/**
 *author: Anmol Vatsa<anvatsa@cs.utah.edu>
 *
 *kernel code to send recv arp request responses
 */

#include "types.h"
#include "defs.h"

int send_arpRequest(const char* interface, const char* ipAddr, char* arpResp) {
  cprintf("Received arp request for ip: %s", ipAddr);
  return 0;
}
