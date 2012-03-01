
.SUFFIXES:	.erl .beam .yrl

.erl.beam:
	erlc -W $<

MODS =	clustmea clustmea_sup \
	clustmea_conf clustmea_task clustmea_reporter \
	misc graphviz

all: compile

compile:	${MODS:%=%.beam}

clean:
	rm -vrf ${MODS:%=%.beam}
	rm -vrf erl_crash.dump
	rm -vrf ${MODS:%=%_SUITE.beam}
	rm -vrf *_SUITE.beam