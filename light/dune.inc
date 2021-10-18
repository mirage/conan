(rule
 (targets ./acorn.ml ./adi.ml ./adventure.ml ./algol68.ml ./allegro.ml
                ./alliant.ml ./alpha.ml ./amanda.ml ./amigaos.ml ./android.ml
                ./animation.ml ./aout.ml ./apache.ml ./apl.ml ./apple.ml
                ./application.ml ./applix.ml ./apt.ml ./archive.ml ./asf.ml
                ./assembler.ml ./asterix.ml ./att3b.ml ./audio.ml ./basis.ml
                ./beetle.ml ./ber.ml ./bflt.ml ./bhl.ml ./bioinformatics.ml
                ./biosig.ml ./blackberry.ml ./blcr.ml ./blender.ml ./blit.ml
                ./bout.ml ./bsdi.ml ./bsi.ml ./btsnoop.ml ./c64.ml
                ./c_lang.ml ./cad.ml ./cafebabe.ml ./cbor.ml ./cddb.ml
                ./chord.ml ./cisco.ml ./citrus.ml ./clarion.ml ./claris.ml
                ./clipper.ml ./clojure.ml ./coff.ml ./commands.ml
                ./communications.ml ./compress.ml ./conan_light.ml
                ./console.ml ./convex.ml ./coverage.ml ./cracklib.ml
                ./ctags.ml ./ctf.ml ./cubemap.ml ./cups.ml ./dact.ml
                ./database.ml ./dataone.ml ./dbpf.ml ./der.ml ./diamond.ml
                ./diff.ml ./digital.ml ./dolby.ml ./dump.ml ./dyadic.ml
                ./ebml.ml ./edid.ml ./editors.ml ./efi.ml ./elf.ml
                ./encore.ml ./epoc.ml ./erlang.ml ./espressif.ml ./esri.ml
                ./etf.ml ./fcs.ml ./filesystems.ml ./finger.ml ./flash.ml
                ./flif.ml ./fonts.ml ./forth.ml ./fortran.ml ./frame.ml
                ./freebsd.ml ./fsav.ml ./fusecompress.ml ./games.ml ./gcc.ml
                ./gconv.ml ./geo.ml ./geos.ml ./gimp.ml ./git.ml ./glibc.ml
                ./gnome.ml ./gnu.ml ./gnumeric.ml ./gpt.ml ./gpu.ml
                ./grace.ml ./graphviz.ml ./gringotts.ml ./guile.ml
                ./hardware.ml ./hitachi_sh.ml ./hp.ml ./human68k.ml
                ./ibm370.ml ./ibm6000.ml ./icc.ml ./iff.ml ./images.ml
                ./inform.ml ./intel.ml ./interleaf.ml ./island.ml ./ispell.ml
                ./isz.ml ./java.ml ./javascript.ml ./jpeg.ml ./karma.ml
                ./kde.ml ./keepass.ml ./kerberos.ml ./kicad.ml ./kml.ml
                ./lecter.ml ./lex.ml ./lif.ml ./linux.ml ./lisp.ml ./llvm.ml
                ./lua.ml ./luks.ml ./m4.ml ./mach.ml ./macintosh.ml
                ./macos.ml ./magic.ml ./mail_news.ml ./make.ml ./map.ml
                ./maple.ml ./marc21.ml ./mathcad.ml ./mathematica.ml
                ./matroska.ml ./mcrypt.ml ./measure.ml ./mercurial.ml
                ./metastore.ml ./meteorological.ml ./microfocus.ml ./mime.ml
                ./mips.ml ./mirage.ml ./misctools.ml ./mkid.ml ./mlssa.ml
                ./mmdf.ml ./modem.ml ./modulefile.ml ./motorola.ml
                ./mozilla.ml ./msdos.ml ./msooxml.ml ./msvc.ml ./msx.ml
                ./mup.ml ./music.ml ./nasa.ml ./natinst.ml ./ncr.ml ./neko.ml
                ./netbsd.ml ./netscape.ml ./netware.ml ./news.ml
                ./nitpicker.ml ./numpy.ml ./oasis.ml ./ocaml.ml ./octave.ml
                ./ole2compounddocs.ml ./olf.ml ./openfst.ml
                ./opentimestamps.ml ./os2.ml ./os400.ml ./os9.ml ./osf1.ml
                ./palm.ml ./parix.ml ./parrot.ml ./pascal.ml ./pbf.ml
                ./pbm.ml ./pc88.ml ./pc98.ml ./pdf.ml ./pdp.ml ./perl.ml
                ./pgf.ml ./pgp.ml ./pkgadd.ml ./plan9.ml ./plus5.ml ./pmem.ml
                ./polyml.ml ./printer.ml ./project.ml ./psdbms.ml ./psl.ml
                ./pulsar.ml ./pwsafe.ml ./pyramid.ml ./python.ml ./qt.ml
                ./revision.ml ./riff.ml ./rinex.ml ./rpi.ml ./rpm.ml
                ./rpmsg.ml ./rst.ml ./rtf.ml ./ruby.ml ./sc.ml ./sccs.ml
                ./scientific.ml ./securitycerts.ml ./selinux.ml ./sendmail.ml
                ./sequent.ml ./sereal.ml ./sgi.ml ./sgml.ml ./sharc.ml
                ./sinclair.ml ./sisu.ml ./sketch.ml ./smalltalk.ml ./smile.ml
                ./sniffer.ml ./softquad.ml ./sosi.ml ./spec.ml ./spectrum.ml
                ./sql.ml ./ssh.ml ./ssl.ml ./sun.ml ./symbos.ml ./sysex.ml
                ./tcl.ml ./teapot.ml ./terminfo.ml ./tex.ml ./tgif.ml
                ./ti_8x.ml ./timezone.ml ./tplink.ml ./troff.ml ./tuxedo.ml
                ./typeset.ml ./unicode.ml ./unknown.ml ./uterus.ml
                ./uuencode.ml ./vacuum_cleaner.ml ./varied_out.ml
                ./varied_script.ml ./vax.ml ./vicar.ml ./virtual'.ml
                ./virtutech.ml ./visx.ml ./vms.ml ./vmware.ml ./vorbis.ml
                ./vxl.ml ./warc.ml ./weak.ml ./webassembly.ml ./windows.ml
                ./wireless.ml ./wordprocessors.ml ./wsdl.ml ./x68000.ml
                ./xdelta.ml ./xenix.ml ./xilinx.ml ./xo65.ml ./xwindows.ml
                ./yara.ml ./zfs.ml ./zilog.ml ./zip.ml ./zyxel.ml)
 (deps %{bin:conan.serialize} (source_tree ../database))
 (action
  (run conan.serialize --only-mime --name conan_light.ml ../database -o .)))

(library
 (name conan_light)
 (public_name conan-database.light)
 (wrapped false)
 (modules 
acorn adi adventure algol68 allegro alliant alpha amanda amigaos android
animation aout apache apl apple application applix apt archive asf assembler
asterix att3b audio basis beetle ber bflt bhl bioinformatics biosig
blackberry blcr blender blit bout bsdi bsi btsnoop c64 c_lang cad cafebabe
cbor cddb chord cisco citrus clarion claris clipper clojure coff commands
communications compress conan_light console convex coverage cracklib ctags
ctf cubemap cups dact database dataone dbpf der diamond diff digital dolby
dump dyadic ebml edid editors efi elf encore epoc erlang espressif esri etf
fcs filesystems finger flash flif fonts forth fortran frame freebsd fsav
fusecompress games gcc gconv geo geos gimp git glibc gnome gnu gnumeric gpt
gpu grace graphviz gringotts guile hardware hitachi_sh hp human68k ibm370
ibm6000 icc iff images inform intel interleaf island ispell isz java
javascript jpeg karma kde keepass kerberos kicad kml lecter lex lif linux
lisp llvm lua luks m4 mach macintosh macos magic mail_news make map maple
marc21 mathcad mathematica matroska mcrypt measure mercurial metastore
meteorological microfocus mime mips mirage misctools mkid mlssa mmdf modem
modulefile motorola mozilla msdos msooxml msvc msx mup music nasa natinst ncr
neko netbsd netscape netware news nitpicker numpy oasis ocaml octave
ole2compounddocs olf openfst opentimestamps os2 os400 os9 osf1 palm parix
parrot pascal pbf pbm pc88 pc98 pdf pdp perl pgf pgp pkgadd plan9 plus5 pmem
polyml printer project psdbms psl pulsar pwsafe pyramid python qt revision
riff rinex rpi rpm rpmsg rst rtf ruby sc sccs scientific securitycerts
selinux sendmail sequent sereal sgi sgml sharc sinclair sisu sketch smalltalk
smile sniffer softquad sosi spec spectrum sql ssh ssl sun symbos sysex tcl
teapot terminfo tex tgif ti_8x timezone tplink troff tuxedo typeset unicode
unknown uterus uuencode vacuum_cleaner varied_out varied_script vax vicar
virtual' virtutech visx vms vmware vorbis vxl warc weak webassembly windows
wireless wordprocessors wsdl x68000 xdelta xenix xilinx xo65 xwindows yara
zfs zilog zip zyxel)
 (libraries conan))
