# Makefile.in generated by automake 1.14.1 from Makefile.am.
# src/Makefile.  Generated from Makefile.in by configure.

# Copyright (C) 1994-2013 Free Software Foundation, Inc.

# This Makefile.in is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.




am__is_gnu_make = test -n '$(MAKEFILE_LIST)' && test -n '$(MAKELEVEL)'
am__make_running_with_option = \
  case $${target_option-} in \
      ?) ;; \
      *) echo "am__make_running_with_option: internal error: invalid" \
              "target option '$${target_option-}' specified" >&2; \
         exit 1;; \
  esac; \
  has_opt=no; \
  sane_makeflags=$$MAKEFLAGS; \
  if $(am__is_gnu_make); then \
    sane_makeflags=$$MFLAGS; \
  else \
    case $$MAKEFLAGS in \
      *\\[\ \	]*) \
        bs=\\; \
        sane_makeflags=`printf '%s\n' "$$MAKEFLAGS" \
          | sed "s/$$bs$$bs[$$bs $$bs	]*//g"`;; \
    esac; \
  fi; \
  skip_next=no; \
  strip_trailopt () \
  { \
    flg=`printf '%s\n' "$$flg" | sed "s/$$1.*$$//"`; \
  }; \
  for flg in $$sane_makeflags; do \
    test $$skip_next = yes && { skip_next=no; continue; }; \
    case $$flg in \
      *=*|--*) continue;; \
        -*I) strip_trailopt 'I'; skip_next=yes;; \
      -*I?*) strip_trailopt 'I';; \
        -*O) strip_trailopt 'O'; skip_next=yes;; \
      -*O?*) strip_trailopt 'O';; \
        -*l) strip_trailopt 'l'; skip_next=yes;; \
      -*l?*) strip_trailopt 'l';; \
      -[dEDm]) skip_next=yes;; \
      -[JT]) skip_next=yes;; \
    esac; \
    case $$flg in \
      *$$target_option*) has_opt=yes; break;; \
    esac; \
  done; \
  test $$has_opt = yes
am__make_dryrun = (target_option=n; $(am__make_running_with_option))
am__make_keepgoing = (target_option=k; $(am__make_running_with_option))
pkgdatadir = $(datadir)/mcf
pkgincludedir = $(includedir)/mcf
pkglibdir = $(libdir)/mcf
pkglibexecdir = $(libexecdir)/mcf
am__cd = CDPATH="$${ZSH_VERSION+.}$(PATH_SEPARATOR)" && cd
install_sh_DATA = $(install_sh) -c -m 644
install_sh_PROGRAM = $(install_sh) -c
install_sh_SCRIPT = $(install_sh) -c
INSTALL_HEADER = $(INSTALL_DATA)
transform = $(program_transform_name)
NORMAL_INSTALL = :
PRE_INSTALL = :
POST_INSTALL = :
NORMAL_UNINSTALL = :
PRE_UNINSTALL = :
POST_UNINSTALL = :
bin_PROGRAMS = mcf$(EXEEXT)
subdir = src
DIST_COMMON = $(srcdir)/Makefile.in $(srcdir)/Makefile.am
ACLOCAL_M4 = $(top_srcdir)/aclocal.m4
am__aclocal_m4_deps = $(top_srcdir)/m4/acx_mpi.m4 \
	$(top_srcdir)/configure.ac
am__configure_deps = $(am__aclocal_m4_deps) $(CONFIGURE_DEPENDENCIES) \
	$(ACLOCAL_M4)
mkinstalldirs = $(install_sh) -d
CONFIG_HEADER = $(top_builddir)/config.h
CONFIG_CLEAN_FILES =
CONFIG_CLEAN_VPATH_FILES =
am__installdirs = "$(DESTDIR)$(bindir)"
PROGRAMS = $(bin_PROGRAMS)
am_mcf_OBJECTS = mcf.$(OBJEXT) mcf_header.$(OBJEXT) \
	Class_Tool.$(OBJEXT) Class_Random.$(OBJEXT) \
	Class_Debug.$(OBJEXT) Class_Control.$(OBJEXT) \
	Class_Boundary.$(OBJEXT) Class_Colloid.$(OBJEXT) \
	Class_Physics.$(OBJEXT) Class_Rhs.$(OBJEXT) Class_IO.$(OBJEXT) \
	Class_StateEquation.$(OBJEXT) Class_Kernel.$(OBJEXT) \
	Class_Technique.$(OBJEXT) Class_Particles.$(OBJEXT) \
	Class_Statistic.$(OBJEXT) Class_Marching.$(OBJEXT)
mcf_OBJECTS = $(am_mcf_OBJECTS)
mcf_LDADD = $(LDADD)
AM_V_P = $(am__v_P_$(V))
am__v_P_ = $(am__v_P_$(AM_DEFAULT_VERBOSITY))
am__v_P_0 = false
am__v_P_1 = :
AM_V_GEN = $(am__v_GEN_$(V))
am__v_GEN_ = $(am__v_GEN_$(AM_DEFAULT_VERBOSITY))
am__v_GEN_0 = @echo "  GEN     " $@;
am__v_GEN_1 = 
AM_V_at = $(am__v_at_$(V))
am__v_at_ = $(am__v_at_$(AM_DEFAULT_VERBOSITY))
am__v_at_0 = @
am__v_at_1 = 
DEFAULT_INCLUDES = -I. -I$(top_builddir)
PPFCCOMPILE = $(FC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_FCFLAGS) $(FCFLAGS)
AM_V_PPFC = $(am__v_PPFC_$(V))
am__v_PPFC_ = $(am__v_PPFC_$(AM_DEFAULT_VERBOSITY))
am__v_PPFC_0 = @echo "  PPFC    " $@;
am__v_PPFC_1 = 
FCLD = $(FC)
FCLINK = $(FCLD) $(AM_FCFLAGS) $(FCFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o \
	$@
AM_V_FCLD = $(am__v_FCLD_$(V))
am__v_FCLD_ = $(am__v_FCLD_$(AM_DEFAULT_VERBOSITY))
am__v_FCLD_0 = @echo "  FCLD    " $@;
am__v_FCLD_1 = 
SOURCES = $(mcf_SOURCES)
DIST_SOURCES = $(mcf_SOURCES)
am__can_run_installinfo = \
  case $$AM_UPDATE_INFO_DIR in \
    n|no|NO) false;; \
    *) (install-info --version) >/dev/null 2>&1;; \
  esac
am__tagged_files = $(HEADERS) $(SOURCES) $(TAGS_FILES) $(LISP)
# Read a list of newline-separated strings from the standard input,
# and print each of them once, without duplicates.  Input order is
# *not* preserved.
am__uniquify_input = $(AWK) '\
  BEGIN { nonempty = 0; } \
  { items[$$0] = 1; nonempty = 1; } \
  END { if (nonempty) { for (i in items) print i; }; } \
'
# Make sure the list of sources is unique.  This is necessary because,
# e.g., the same source file might be shared among _SOURCES variables
# for different programs/libraries.
am__define_uniq_tagged_files = \
  list='$(am__tagged_files)'; \
  unique=`for i in $$list; do \
    if test -f "$$i"; then echo $$i; else echo $(srcdir)/$$i; fi; \
  done | $(am__uniquify_input)`
ETAGS = etags
CTAGS = ctags
DISTFILES = $(DIST_COMMON) $(DIST_SOURCES) $(TEXINFOS) $(EXTRA_DIST)
ACLOCAL = ${SHELL} /dipc/lsantelli/MCF-intel-2/MCF-run-3d/mcf/missing aclocal-1.14
AMTAR = $${TAR-tar}
AM_DEFAULT_VERBOSITY = 1
AUTOCONF = ${SHELL} /dipc/lsantelli/MCF-intel-2/MCF-run-3d/mcf/missing autoconf
AUTOHEADER = ${SHELL} /dipc/lsantelli/MCF-intel-2/MCF-run-3d/mcf/missing autoheader
AUTOMAKE = ${SHELL} /dipc/lsantelli/MCF-intel-2/MCF-run-3d/mcf/missing automake-1.14
AWK = gawk
CC = gcc
CCDEPMODE = depmode=none
CFLAGS = -g -O2
CPPFLAGS = 
CXX = g++
CXXDEPMODE = depmode=none
CXXFLAGS = -g -O2
CYGPATH_W = echo
DEFS = -DHAVE_CONFIG_H
DEPDIR = .deps
ECHO_C = 
ECHO_N = -n
ECHO_T = 
EXEEXT = 
FC = mpif90
FCFLAGS = -I/dipc/lsantelli/prefix-mcf/include -g -O2
FCFLAGS_F90 = 
INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_PROGRAM = ${INSTALL}
INSTALL_SCRIPT = ${INSTALL}
INSTALL_STRIP_PROGRAM = $(install_sh) -c -s
LDFLAGS = -L/dipc/lsantelli/prefix-mcf/lib/
LIBOBJS = 
LIBS = -lppm -lmetis -lvizing -lfftw3f -lfftw3 -lgcc_s -lstdc++   
LTLIBOBJS = 
MAKEDEPF90 = /dipc/lsantelli/prefix-mcf/bin/makedepf90
MAKEINFO = ${SHELL} /dipc/lsantelli/MCF-intel-2/MCF-run-3d/mcf/missing makeinfo
MKDIR_P = /usr/bin/mkdir -p
MPICC = mpicc
MPILIBS =  
OBJEXT = o
PACKAGE = mcf
PACKAGE_BUGREPORT = xin.bian@aer.mw.tum.de
PACKAGE_NAME = mcf
PACKAGE_STRING = mcf 1.0
PACKAGE_TARNAME = mcf
PACKAGE_URL = 
PACKAGE_VERSION = 1.0
PATH_SEPARATOR = :
SET_MAKE = 
SHELL = /bin/sh
STRIP = 
VERSION = 1.0
abs_builddir = /dipc/lsantelli/MCF-intel-2/MCF-run-3d/mcf/src
abs_srcdir = /dipc/lsantelli/MCF-intel-2/MCF-run-3d/mcf/src
abs_top_builddir = /dipc/lsantelli/MCF-intel-2/MCF-run-3d/mcf
abs_top_srcdir = /dipc/lsantelli/MCF-intel-2/MCF-run-3d/mcf
ac_ct_CC = gcc
ac_ct_CXX = g++
ac_ct_FC = 
am__include = include
am__leading_dot = .
am__quote = 
am__tar = $${TAR-tar} chof - "$$tardir"
am__untar = $${TAR-tar} xf -
bindir = ${exec_prefix}/bin
build_alias = 
builddir = .
datadir = ${datarootdir}
datarootdir = ${prefix}/share
docdir = ${datarootdir}/doc/${PACKAGE_TARNAME}
dvidir = ${docdir}
exec_prefix = ${prefix}
host_alias = 
htmldir = ${docdir}
includedir = ${prefix}/include
infodir = ${datarootdir}/info
install_sh = ${SHELL} /dipc/lsantelli/MCF-intel-2/MCF-run-3d/mcf/install-sh
libdir = ${exec_prefix}/lib
libexecdir = ${exec_prefix}/libexec
localedir = ${datarootdir}/locale
localstatedir = ${prefix}/var
mandir = ${datarootdir}/man
mkdir_p = $(MKDIR_P)
oldincludedir = /usr/include
pdfdir = ${docdir}
prefix = /usr/local
program_transform_name = s,x,x,
psdir = ${docdir}
sbindir = ${exec_prefix}/sbin
sharedstatedir = ${prefix}/com
srcdir = .
sysconfdir = ${prefix}/etc
target_alias = 
top_build_prefix = ../
top_builddir = ..
top_srcdir = ..
AM_FCFLAGS = -D__MPI -I ${top_srcdir}/src
mcf_SOURCES = mcf.F90\
	       mcf_header.F90\
	       Class_Tool.F90\
	       Class_Random.F90\
	       Class_Debug.F90\
	       Class_Control.F90\
               Class_Boundary.F90\
               Class_Colloid.F90\
	       Class_Physics.F90\
               Class_Rhs.F90\
	       Class_IO.F90\
	       Class_StateEquation.F90\
	       Class_Kernel.F90\
               Class_Technique.F90\
	       Class_Particles.F90\
	       Class_Statistic.F90\
	       Class_Marching.F90

EXTRA_DIST = \
	boundary_check_wall_solid_particle.F90 boundary_collect_colloid_interaction.F90 boundary_collect_particles_interaction.F90 \
	boundary_finalize.F90 boundary_get.F90 boundary_new.F90 \
	boundary_noslip_mirror.F90 boundary_noslip_solid.F90 boundary_set.F90 \
	boundary_update_boundary.F90 colloid_adjust_colloid.F90 colloid_adjust_parameters.F90 \
	colloid_apply_body_force.F90 colloid_cartesian_ellipse.F90 colloid_cartesian_ellipsoid.F90 \
	colloid_check_boundary_particle.F90 colloid_check_parameters.F90 colloid_check_penetration.F90 \
	colloid_collect_particles_interaction.F90 colloid_compute_acceleration.F90 colloid_compute_accumulation_matrix.F90 \
	colloid_compute_accumulation_vector.F90 colloid_compute_dt_f.F90 colloid_compute_image.F90 \
	colloid_compute_interaction.F90 colloid_compute_lubrication_cc.F90 colloid_compute_lubrication_cw.F90 \
	colloid_compute_repulsion_cc.F90 colloid_compute_repulsion_cw.F90 colloid_compute_rotation_matrix.F90 \
	colloid_compute_rotation_vector.F90 colloid_compute_statistic.F90 colloid_create_boundary_particle_2D.F90 \
	colloid_create_boundary_particle_3D.F90 colloid_create_boundary_particle_3D_dicolloid.F90 colloid_create_boundary_particle_3D_sphere.F90 \
	colloid_finalize.F90 colloid_find_force_extreme.F90 colloid_get.F90 \
	colloid_in_nearest_image.F90 colloid_in_relative_position.F90 colloid_init_accumulation_matrix.F90 \
	colloid_initialize_image.F90 colloid_integrate_position.F90 colloid_integrate_velocity.F90 \
	colloid_nearest_image.F90 colloid_new.F90 colloid_noslip.F90 \
	colloid_noslip_Morris.F90 colloid_noslip_Morris_cylinder_2D.F90 colloid_noslip_Morris_cylinder_3D.F90 \
	colloid_noslip_Morris_dicolloid_3D.F90 colloid_noslip_Morris_disk.F90 colloid_noslip_Morris_ellipse.F90 \
	colloid_noslip_Morris_ellipsoid.F90 colloid_noslip_Morris_sphere.F90 colloid_noslip_Morris_star.F90 \
	colloid_noslip_Morris_star_2D.F90 colloid_noslip_Zhu.F90 colloid_noslip_frozen.F90 \
	colloid_particle_velocity.F90 colloid_polar_ellipse.F90 colloid_polar_star.F90 \
	colloid_set.F90 colloid_set_flow_developed.F90 colloid_spherical_ellipsoid.F90 \
	control_check_parameters.F90 control_finalize.F90 control_get.F90 \
	control_new.F90 control_set.F90 debug_close.F90 \
	debug_finalize.F90 debug_get.F90 debug_new.F90 \
	debug_open.F90 debug_print_msg.F90 debug_set.F90 \
	debug_substart.F90 debug_substop.F90 debug_validate_motion.F90 \
	debug_write_output.F90 debug_write_time.F90 io_adjust_parameters.F90 \
	io_check_parameters.F90 io_close.F90 io_finalize.F90 \
	io_get.F90 io_new.F90 io_open.F90 \
	io_read.F90 io_read_conformation.F90 io_read_ctrl.F90 \
	io_read_io.F90 io_read_particles.F90 io_read_physics.F90 \
	io_write.F90 io_write_boundary.F90 io_write_colloid.F90 \
	io_write_colloid_separate.F90 io_write_condition.F90 io_write_conformation.F90 \
	io_write_particles.F90 io_write_particles_relax.F90 io_write_restart_conformation.F90 \
	io_write_restart_particles.F90 io_write_restart_particles_relax.F90 io_write_restart_physics.F90 \
	io_write_statistic.F90 io_write_statistic_relax.F90 kernel_finalize.F90 \
	kernel_get.F90 kernel_kernel.F90 kernel_kernel_Lucy.F90 \
	kernel_kernel_quintic_spline.F90 kernel_new.F90 marching_adjust_flow_v.F90 \
	marching_finalize.F90 marching_integrate.F90 marching_integrate_Euler.F90 \
	marching_integrate_VV.F90 marching_marching.F90 marching_new.F90 \
	marching_relax.F90 mcf.F90 mcf_header.F90 \
	particles_adjust_particles.F90 particles_apply_body_force.F90 particles_collect_boundary_interaction.F90 \
	particles_collect_colloid_interaction.F90 particles_compute_act.F90 particles_compute_aeval.F90 \
	particles_compute_aevec.F90 particles_compute_colloid_absolute_position.F90 particles_compute_colloid_relative_position.F90 \
	particles_compute_ct.F90 particles_compute_density.F90 particles_compute_dt_f.F90 \
	particles_compute_evgt.F90 particles_compute_interaction.F90 particles_compute_mass.F90 \
	particles_compute_pressure.F90 particles_compute_pressure_tensor.F90 particles_decompose_global.F90 \
	particles_decompose_partial.F90 particles_finalize.F90 particles_find_density_extreme.F90 \
	particles_find_force_extreme.F90 particles_get.F90 particles_init_global_assign_id.F90 \
	particles_init_global_exter.F90 particles_init_global_inter.F90 particles_init_global_inter_cubic.F90 \
	particles_init_global_inter_hexagonal.F90 particles_init_global_inter_square.F90 particles_init_global_inter_staggered.F90 \
	particles_init_partial_exter.F90 particles_init_partial_inter.F90 particles_integrate_boundary_position.F90 \
	particles_integrate_ct.F90 particles_integrate_eval.F90 particles_integrate_evec.F90 \
	particles_integrate_position.F90 particles_integrate_potential_energy.F90 particles_integrate_velocity.F90 \
	particles_map_ghost_get.F90 particles_map_ghost_put.F90 particles_new.F90 \
	particles_normalize_density.F90 particles_reset.F90 particles_reset_boundary_ghost_interaction.F90 \
	particles_reset_boundary_interaction.F90 particles_reset_boundary_velocity.F90 particles_reset_colloid_interaction.F90 \
	particles_reset_colloid_velocity.F90 particles_set.F90 particles_set_boundary_ghost_id.F90 \
	particles_set_boundary_ghost_velocity.F90 particles_set_boundary_velocity.F90 particles_set_colloid_on_lattice.F90 \
	particles_set_colloid_velocity.F90 particles_set_flow_developed.F90 physics_adapt_dt.F90 \
	physics_adjust_parameters.F90 physics_check_parameters.F90 physics_finalize.F90 \
	physics_get.F90 physics_initialize_dt.F90 physics_new.F90 \
	physics_set.F90 random_new.F90 random_random.F90 \
	random_random_Gaussian.F90 random_random_uniform.F90 rhs_density.F90 \
	rhs_finalize.F90 rhs_force_cc_Newtonian.F90 rhs_force_ff_Newtonian.F90 \
	rhs_force_ff_Newtonian_Espanol.F90 rhs_force_ff_Newtonian_HuAdams.F90 rhs_force_ff_Newtonian_HuAdams_angular.F90 \
	rhs_force_ff_Newtonian_Morris.F90 rhs_force_ff_non_Newtonian.F90 rhs_force_ff_non_Newtonian_Espanol.F90 \
	rhs_force_ff_non_Newtonian_HuAdams.F90 rhs_force_ff_non_Newtonian_HuAdams_angular.F90 rhs_get.F90 \
	rhs_new.F90 rhs_set.F90 stateEquation_compute_pressure.F90 \
	stateEquation_finalize.F90 stateEquation_get.F90 stateEquation_new.F90 \
	stateEquation_set.F90 statistic_compute_disorder.F90 statistic_compute_statistic.F90 \
	statistic_compute_v_average.F90 statistic_finalize.F90 statistic_get.F90 \
	statistic_new.F90 statistic_set.F90 technique_build_list.F90 \
	technique_finalize.F90 technique_get.F90 technique_new.F90 \
	tool_cross_product.F90 tool_new.F90 tool_rotation_matrix.F90 \
	tool_rotation_vector.F90 tool_uppercase.F90\
	pp_interaction.inc pp_interaction_cc.inc pp_interaction_cf.inc \
	pp_interaction_cw.inc pp_interaction_fc.inc pp_interaction_ff.inc \
	pp_interaction_fw.inc pp_interaction_wc.inc pp_interaction_wf.inc \
	pp_vgt_ip.inc pp_vgt_jp.inc ppm_param.inc

all: all-am

.SUFFIXES:
.SUFFIXES: .F90 .o .obj
$(srcdir)/Makefile.in:  $(srcdir)/Makefile.am  $(am__configure_deps)
	@for dep in $?; do \
	  case '$(am__configure_deps)' in \
	    *$$dep*) \
	      ( cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) am--refresh ) \
	        && { if test -f $@; then exit 0; else break; fi; }; \
	      exit 1;; \
	  esac; \
	done; \
	echo ' cd $(top_srcdir) && $(AUTOMAKE) --foreign src/Makefile'; \
	$(am__cd) $(top_srcdir) && \
	  $(AUTOMAKE) --foreign src/Makefile
.PRECIOUS: Makefile
Makefile: $(srcdir)/Makefile.in $(top_builddir)/config.status
	@case '$?' in \
	  *config.status*) \
	    cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) am--refresh;; \
	  *) \
	    echo ' cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@ $(am__depfiles_maybe)'; \
	    cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@ $(am__depfiles_maybe);; \
	esac;

$(top_builddir)/config.status: $(top_srcdir)/configure $(CONFIG_STATUS_DEPENDENCIES)
	cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) am--refresh

$(top_srcdir)/configure:  $(am__configure_deps)
	cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) am--refresh
$(ACLOCAL_M4):  $(am__aclocal_m4_deps)
	cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) am--refresh
$(am__aclocal_m4_deps):
install-binPROGRAMS: $(bin_PROGRAMS)
	@$(NORMAL_INSTALL)
	@list='$(bin_PROGRAMS)'; test -n "$(bindir)" || list=; \
	if test -n "$$list"; then \
	  echo " $(MKDIR_P) '$(DESTDIR)$(bindir)'"; \
	  $(MKDIR_P) "$(DESTDIR)$(bindir)" || exit 1; \
	fi; \
	for p in $$list; do echo "$$p $$p"; done | \
	sed 's/$(EXEEXT)$$//' | \
	while read p p1; do if test -f $$p \
	  ; then echo "$$p"; echo "$$p"; else :; fi; \
	done | \
	sed -e 'p;s,.*/,,;n;h' \
	    -e 's|.*|.|' \
	    -e 'p;x;s,.*/,,;s/$(EXEEXT)$$//;$(transform);s/$$/$(EXEEXT)/' | \
	sed 'N;N;N;s,\n, ,g' | \
	$(AWK) 'BEGIN { files["."] = ""; dirs["."] = 1 } \
	  { d=$$3; if (dirs[d] != 1) { print "d", d; dirs[d] = 1 } \
	    if ($$2 == $$4) files[d] = files[d] " " $$1; \
	    else { print "f", $$3 "/" $$4, $$1; } } \
	  END { for (d in files) print "f", d, files[d] }' | \
	while read type dir files; do \
	    if test "$$dir" = .; then dir=; else dir=/$$dir; fi; \
	    test -z "$$files" || { \
	      echo " $(INSTALL_PROGRAM_ENV) $(INSTALL_PROGRAM) $$files '$(DESTDIR)$(bindir)$$dir'"; \
	      $(INSTALL_PROGRAM_ENV) $(INSTALL_PROGRAM) $$files "$(DESTDIR)$(bindir)$$dir" || exit $$?; \
	    } \
	; done

uninstall-binPROGRAMS:
	@$(NORMAL_UNINSTALL)
	@list='$(bin_PROGRAMS)'; test -n "$(bindir)" || list=; \
	files=`for p in $$list; do echo "$$p"; done | \
	  sed -e 'h;s,^.*/,,;s/$(EXEEXT)$$//;$(transform)' \
	      -e 's/$$/$(EXEEXT)/' \
	`; \
	test -n "$$list" || exit 0; \
	echo " ( cd '$(DESTDIR)$(bindir)' && rm -f" $$files ")"; \
	cd "$(DESTDIR)$(bindir)" && rm -f $$files

clean-binPROGRAMS:
	-test -z "$(bin_PROGRAMS)" || rm -f $(bin_PROGRAMS)

mcf$(EXEEXT): $(mcf_OBJECTS) $(mcf_DEPENDENCIES) $(EXTRA_mcf_DEPENDENCIES) 
	@rm -f mcf$(EXEEXT)
	$(AM_V_FCLD)$(FCLINK) $(mcf_OBJECTS) $(mcf_LDADD) $(LIBS)

mostlyclean-compile:
	-rm -f *.$(OBJEXT)

distclean-compile:
	-rm -f *.tab.c

.F90.o:
	$(AM_V_PPFC)$(PPFCCOMPILE) -c -o $@ $<

.F90.obj:
	$(AM_V_PPFC)$(PPFCCOMPILE) -c -o $@ `$(CYGPATH_W) '$<'`

ID: $(am__tagged_files)
	$(am__define_uniq_tagged_files); mkid -fID $$unique
tags: tags-am
TAGS: tags

tags-am: $(TAGS_DEPENDENCIES) $(am__tagged_files)
	set x; \
	here=`pwd`; \
	$(am__define_uniq_tagged_files); \
	shift; \
	if test -z "$(ETAGS_ARGS)$$*$$unique"; then :; else \
	  test -n "$$unique" || unique=$$empty_fix; \
	  if test $$# -gt 0; then \
	    $(ETAGS) $(ETAGSFLAGS) $(AM_ETAGSFLAGS) $(ETAGS_ARGS) \
	      "$$@" $$unique; \
	  else \
	    $(ETAGS) $(ETAGSFLAGS) $(AM_ETAGSFLAGS) $(ETAGS_ARGS) \
	      $$unique; \
	  fi; \
	fi
ctags: ctags-am

CTAGS: ctags
ctags-am: $(TAGS_DEPENDENCIES) $(am__tagged_files)
	$(am__define_uniq_tagged_files); \
	test -z "$(CTAGS_ARGS)$$unique" \
	  || $(CTAGS) $(CTAGSFLAGS) $(AM_CTAGSFLAGS) $(CTAGS_ARGS) \
	     $$unique

GTAGS:
	here=`$(am__cd) $(top_builddir) && pwd` \
	  && $(am__cd) $(top_srcdir) \
	  && gtags -i $(GTAGS_ARGS) "$$here"
cscopelist: cscopelist-am

cscopelist-am: $(am__tagged_files)
	list='$(am__tagged_files)'; \
	case "$(srcdir)" in \
	  [\\/]* | ?:[\\/]*) sdir="$(srcdir)" ;; \
	  *) sdir=$(subdir)/$(srcdir) ;; \
	esac; \
	for i in $$list; do \
	  if test -f "$$i"; then \
	    echo "$(subdir)/$$i"; \
	  else \
	    echo "$$sdir/$$i"; \
	  fi; \
	done >> $(top_builddir)/cscope.files

distclean-tags:
	-rm -f TAGS ID GTAGS GRTAGS GSYMS GPATH tags

distdir: $(DISTFILES)
	@srcdirstrip=`echo "$(srcdir)" | sed 's/[].[^$$\\*]/\\\\&/g'`; \
	topsrcdirstrip=`echo "$(top_srcdir)" | sed 's/[].[^$$\\*]/\\\\&/g'`; \
	list='$(DISTFILES)'; \
	  dist_files=`for file in $$list; do echo $$file; done | \
	  sed -e "s|^$$srcdirstrip/||;t" \
	      -e "s|^$$topsrcdirstrip/|$(top_builddir)/|;t"`; \
	case $$dist_files in \
	  */*) $(MKDIR_P) `echo "$$dist_files" | \
			   sed '/\//!d;s|^|$(distdir)/|;s,/[^/]*$$,,' | \
			   sort -u` ;; \
	esac; \
	for file in $$dist_files; do \
	  if test -f $$file || test -d $$file; then d=.; else d=$(srcdir); fi; \
	  if test -d $$d/$$file; then \
	    dir=`echo "/$$file" | sed -e 's,/[^/]*$$,,'`; \
	    if test -d "$(distdir)/$$file"; then \
	      find "$(distdir)/$$file" -type d ! -perm -700 -exec chmod u+rwx {} \;; \
	    fi; \
	    if test -d $(srcdir)/$$file && test $$d != $(srcdir); then \
	      cp -fpR $(srcdir)/$$file "$(distdir)$$dir" || exit 1; \
	      find "$(distdir)/$$file" -type d ! -perm -700 -exec chmod u+rwx {} \;; \
	    fi; \
	    cp -fpR $$d/$$file "$(distdir)$$dir" || exit 1; \
	  else \
	    test -f "$(distdir)/$$file" \
	    || cp -p $$d/$$file "$(distdir)/$$file" \
	    || exit 1; \
	  fi; \
	done
check-am: all-am
check: check-am
all-am: Makefile $(PROGRAMS)
installdirs:
	for dir in "$(DESTDIR)$(bindir)"; do \
	  test -z "$$dir" || $(MKDIR_P) "$$dir"; \
	done
install: install-am
install-exec: install-exec-am
install-data: install-data-am
uninstall: uninstall-am

install-am: all-am
	@$(MAKE) $(AM_MAKEFLAGS) install-exec-am install-data-am

installcheck: installcheck-am
install-strip:
	if test -z '$(STRIP)'; then \
	  $(MAKE) $(AM_MAKEFLAGS) INSTALL_PROGRAM="$(INSTALL_STRIP_PROGRAM)" \
	    install_sh_PROGRAM="$(INSTALL_STRIP_PROGRAM)" INSTALL_STRIP_FLAG=-s \
	      install; \
	else \
	  $(MAKE) $(AM_MAKEFLAGS) INSTALL_PROGRAM="$(INSTALL_STRIP_PROGRAM)" \
	    install_sh_PROGRAM="$(INSTALL_STRIP_PROGRAM)" INSTALL_STRIP_FLAG=-s \
	    "INSTALL_PROGRAM_ENV=STRIPPROG='$(STRIP)'" install; \
	fi
mostlyclean-generic:

clean-generic:

distclean-generic:
	-test -z "$(CONFIG_CLEAN_FILES)" || rm -f $(CONFIG_CLEAN_FILES)
	-test . = "$(srcdir)" || test -z "$(CONFIG_CLEAN_VPATH_FILES)" || rm -f $(CONFIG_CLEAN_VPATH_FILES)

maintainer-clean-generic:
	@echo "This command is intended for maintainers to use"
	@echo "it deletes files that may require special tools to rebuild."
clean: clean-am

clean-am: clean-binPROGRAMS clean-generic clean-local mostlyclean-am

distclean: distclean-am
	-rm -f Makefile
distclean-am: clean-am distclean-compile distclean-generic \
	distclean-tags

dvi: dvi-am

dvi-am:

html: html-am

html-am:

info: info-am

info-am:

install-data-am:

install-dvi: install-dvi-am

install-dvi-am:

install-exec-am: install-binPROGRAMS

install-html: install-html-am

install-html-am:

install-info: install-info-am

install-info-am:

install-man:

install-pdf: install-pdf-am

install-pdf-am:

install-ps: install-ps-am

install-ps-am:

installcheck-am:

maintainer-clean: maintainer-clean-am
	-rm -f Makefile
maintainer-clean-am: distclean-am maintainer-clean-generic

mostlyclean: mostlyclean-am

mostlyclean-am: mostlyclean-compile mostlyclean-generic

pdf: pdf-am

pdf-am:

ps: ps-am

ps-am:

uninstall-am: uninstall-binPROGRAMS

.MAKE: install-am install-strip

.PHONY: CTAGS GTAGS TAGS all all-am check check-am clean \
	clean-binPROGRAMS clean-generic clean-local cscopelist-am \
	ctags ctags-am distclean distclean-compile distclean-generic \
	distclean-tags distdir dvi dvi-am html html-am info info-am \
	install install-am install-binPROGRAMS install-data \
	install-data-am install-dvi install-dvi-am install-exec \
	install-exec-am install-html install-html-am install-info \
	install-info-am install-man install-pdf install-pdf-am \
	install-ps install-ps-am install-strip installcheck \
	installcheck-am installdirs maintainer-clean \
	maintainer-clean-generic mostlyclean mostlyclean-compile \
	mostlyclean-generic pdf pdf-am ps ps-am tags tags-am uninstall \
	uninstall-am uninstall-binPROGRAMS


Makefile.dep:
	$(MAKEDEPF90) -I ${top_srcdir}/src $(mcf_SOURCES) > $@

clean-local:
	-rm *.mod Makefile.dep

-include Makefile.dep

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
