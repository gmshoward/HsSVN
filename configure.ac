AC_INIT([HsSVN], [], [phonohawk@ps.sakura.ne.jp])

AC_CONFIG_SRCDIR([HsSVN.cabal])

#
# Checks for Subversion...
#
# FIXME: we can get rid of these ugly checks when we got svn-config or
# something alike.
#
AC_ARG_VAR([EXTRA_CPPFLAGS], [extra CPPFLAGS to find subversion])
AC_ARG_VAR([EXTRA_LDFLAGS], [extra LDFLAGS to find subversion])
CPPFLAGS="$EXTRA_CPPFLAGS $CPPFLAGS"
LDFLAGS="$EXTRA_LDFLAGS $LDFLAGS"

SVN_CFLAGS="$EXTRA_CPPFLAGS"
SVN_LIBS="$EXTRA_LIBS -lsvn_subr-1 -lsvn_repos-1 -lsvn_fs-1"
AC_SUBST([SVN_CFLAGS])
AC_SUBST([SVN_LIBS])

AC_CHECK_HEADERS(
        [svn_error.h],
        [],
        [AC_MSG_ERROR([SVN headers are required. Hint: EXTRA_CPPFLAGS])])

AC_CHECK_LIB(
        [svn_subr-1],
        [svn_error_clear],
        [],
        [AC_MSG_ERROR([SVN libraries are required. Hint: EXTRA_LDFLAGS])])

AC_ARG_VAR([APR_CONFIG], [apr-config to use])
AC_CHECK_PROGS(
        [APR_CONFIG],
        [apr-config],
        [AC_MSG_ERROR([apr-config is required. Hint: APR_CONFIG])])
SVN_CFLAGS=`$APR_CONFIG --includes --cppflags`" $SVN_CFLAGS"
SVN_LIBS=`$APR_CONFIG --link-ld --libs`" $SVN_LIBS"
#
# Done the ugly checks
#

AC_CONFIG_FILES([
        HsSVN.buildinfo
])
AC_OUTPUT