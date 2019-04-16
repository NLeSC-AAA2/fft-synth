OCAMLBUILD = ocamlbuild
GENFFT_NATIVE = gen_notw.native gen_notw_c.native gen_twiddle.native	\
gen_twiddle_c.native gen_twidsq.native gen_twidsq_c.native		\
gen_r2r.native gen_r2cf.native gen_r2cb.native gen_hc2c.native		\
gen_hc2cdft.native gen_hc2cdft_c.native gen_hc2hc.native		\
gen_mdct.native gen_notw_complex.native gen_twiddle_complex.native

.PHONY: all clean

all:
	$(OCAMLBUILD) -classic-display -libs unix,nums $(GENFFT_NATIVE)

clean:
	$(OCAMLBUILD) -classic-display -clean

