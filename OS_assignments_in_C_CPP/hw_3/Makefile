PTHREADS_TSP = pthreads_tsp.cpp
BWTSP = busywait_tsp.cpp atomic_ops.h
BWTSPOPT = -c -pthread -DX86 -m32 -msse2
EXALL = function_call system_call context_switch pthreads_tsp busywait_tsp

all: $(EXALL)

function_call: function_call.o hrtimer_x86.o
	g++ function_call.o hrtimer_x86.o -o function_call

system_call: system_call.o hrtimer_x86.o
	g++ system_call.o hrtimer_x86.o -o system_call

context_switch: context_switch.o hrtimer_x86.o
	g++ context_switch.o hrtimer_x86.o -o context_switch

pthreads_tsp: pthreads_tsp.o hrtimer_x86.o
	g++ pthreads_tsp.o hrtimer_x86.o -pthread -o pthreads_tsp

busywait_tsp: busywait_tsp.o hrtimer_x86.o
	g++ busywait_tsp.o hrtimer_x86.o -pthread -o busywait_tsp


function_call.o: function_call.cpp
	g++ -c function_call.cpp

system_call.o: system_call.cpp
	g++ -c system_call.cpp

context_switch.o: context_switch.cpp
	g++ -c context_switch.cpp

pthreads_tsp.o: $(PTHREADS_TSP)
	g++ -c -pthread $(PTHREADS_TSP)

busywait_tsp.o: $(BWTSP)
	g++ $(BWTSPOPT) $(BWTSP)


hrtimer_x86.o: hrtimer_x86.cc hrtimer_x86.h
	g++ -c hrtimer_x86.cc hrtimer_x86.h

clean:
	rm -rf *o $(EXALL)
