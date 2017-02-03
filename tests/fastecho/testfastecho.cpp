/* Copyright (c) 2017 Connectal Project
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#include <errno.h>
#include <stdio.h>
#include <signal.h>
#include <iostream>
#include "FastEchoIndication.h"
#include "FastEchoRequest.h"
#include "GeneratedTypes.h"

class FastEcho : public FastEchoIndicationWrapper
{
    public:
        FastEcho(unsigned int indicationId, unsigned int requestId)
                : FastEchoIndicationWrapper(indicationId),
                  fastEchoRequestProxy(requestId) {
            sem_init(&sem, 1, 0);
        }

        virtual void indication(uint64_t x) {
            resp = x;
            sem_post(&sem);
        }

        bool doEcho(uint64_t x) {
            fastEchoRequestProxy.request(x);
            sem_wait(&sem);
            if (resp != x) {
                std::cerr << "ERROR: echo failed" << std::endl;
                return false;
            } else {
                return true;
            }
        }

        void clearSemaphore() {
            sem_post(&sem);
        }

    private:
        FastEchoRequestProxy fastEchoRequestProxy;
        sem_t sem;
        uint64_t resp;
};

FastEcho *fastEcho;

int reset_attempts = 0;
const int max_reset_attempts = 5;

void handle_signal(int sig) {
    signal(SIGINT, &handle_signal);
    if (reset_attempts >= max_reset_attempts) {
        std::cerr << ">> Max reset attempts reached" << std::endl;
        std::cerr << ">> Exiting" << std::endl;
        exit(1);
    } else if (fastEcho != NULL) {
        reset_attempts++;
        std::cerr << ">> Clearing Semaphore" << std::endl;
        fastEcho->clearSemaphore();
    } else {
        std::cerr << ">> fastEcho == NULL" << std::endl;
        std::cerr << ">> Exiting" << std::endl;
        exit(1);
    }
    return;
}

int main(int argc, const char **argv)
{
    long actualFrequency = 0;
    long requestedFrequency = 1e9 / MainClockPeriod;

    int status = setClockFrequency(0, requestedFrequency, &actualFrequency);
    fprintf(stderr, "Requested main clock frequency %5.2f, actual clock frequency %5.2f MHz status=%d errno=%d\n",
	    (double)requestedFrequency * 1.0e-6,
	    (double)actualFrequency * 1.0e-6,
	    status, (status != 0) ? errno : 0);

    signal(SIGINT, &handle_signal);

    fastEcho = new FastEcho(IfcNames_FastEchoIndicationH2S, IfcNames_FastEchoRequestS2H);

    for (uint64_t i = 0 ; i < 1000000 ; i++) {
        if (i % 1000 == 0) {
            std::cout << "i = " << i << std::endl;
        }
        fastEcho->doEcho(i);
    }

    std::cout << "Done" << std::endl;

    return 0;
}
