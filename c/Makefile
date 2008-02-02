# Makefile for Boolean Network Simulation

# Definition
CC=		gcc
LD=		ld -shared
FLAGS=		-Wall -DUSE_GSL `gsl-config --cflags` -g -O2
LIBS=		`gsl-config --libs` -lm
ENGINE_OBJS=	utility.o point.o rule.o node.o net.o ev.o rbn.o
SEARCH_OBJS=	attractor.o attractorSet.o lab.o main.o

# Rule
.SUFFIXES: .o .c
.c.o:
	$(CC) -c $< $(FLAGS)

# Make a exetuable.
bns: $(ENGINE_OBJS) $(SEARCH_OBJS)
	$(CC) -o $@ $(ENGINE_OBJS) $(SEARCH_OBJS) $(LIBS) 

# Make a shared library (ENGINE only).
libbns.so: $(OBJS) 
	$(LD) -o $@ $(ENGINE_OBJS) $(LIBS)

# Remove objects files
clean: 
	$(RM) $(ENGINE_OBJS) $(SEARCH_OBJS)

# Engine
utility.o : utility.c header.h
point.o : point.c point.h
rule.o : rule.c rule.h
node.o : node.c node.h
net.o : net.c net.h
ev.o : ev.c ev.h
rbn.o : rbn.h rbn.c

# Search
attractor.o: attractor.c attractor.h
attractorSet.o: attractorSet.c attractorSet.h
lab.o: lab.c lab.h
main.o: main.c



