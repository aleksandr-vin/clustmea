
.SUFFIXES:	.erl .beam .yrl

.erl.beam:
	erlc -W -pa ./ $<

BEHEVIOURS = \
	gen_uploader

MODS =	clustmea clustmea_sup \
	clustmea_conf clustmea_task clustmea_reporter \
	my_uploader riak_inets_uploader \
	kv_producers \
	misc graphviz

all: compile

compile:	${BEHEVIOURS:%=%.beam} ${MODS:%=%.beam}

clean:
	rm -vrf ${MODS:%=%.beam}
	rm -vrf ${BEHEVIOURS:%=%.beam}
	rm -vrf ${MODS:%=%_SUITE.beam}
	rm -vrf ${BEHEVIOURS:%=%_SUITE.beam}
	rm -vrf *_SUITE.beam
	rm -vrf erl_crash.dump