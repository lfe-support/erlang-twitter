"""
erlang-twitter

@author: Jean-Lou Dupont
"""
import os
import shutil

from helpers import *

all = [
		{'src': './ebin',    'dst':'./packages/debian/usr/lib/erlang/lib/twitter/ebin'},
		{'src': './include', 'dst':'./packages/debian/usr/lib/erlang/lib/twitter/include'},
		]

print "Preparing .deb package"
try:
	version = read_version()
	print """> building version [%s]""" % version
	
	print """> cloning files to packages/debian"""

	for one in all:
		src, dst = one
		#release
		print "> cloning [%s]" % (src % ('release', ''))
		shutil.copy(	src % ('release', ''), 
						dst % ('',        version) )
	
	print """> removing /tmp directory"""
	shutil.rmtree('/tmp/%s_deb' % this_lib, ignore_errors=True)

	print """scons: cloning ./packages/debian to /tmp directory"""
	safe_copytree('../packages/debian', '/tmp/%s_deb' % this_lib, skip_dirs=['.svn',], dir_mode=0775, make_dirs=True)

	print """scons: adjusting version in control files"""
	c_path = '/tmp/%s_deb/DEBIAN' % this_lib
	params = {'version':version}
	adjust_control_files(params, c_path)
	
	print """scons: adjusting permissions for `dkpg-deb` command-line"""
	recursive_chmod("/tmp/%s_deb" % this_lib, mode=0775)

	print """scons: copying beam files"""
	safe_copytree("./epem/ebin", "/tmp/%s_deb/usr/share/pem/bin" % this_lib)

	print """scons: copying pem.py to pem & setting permissions"""
	path="/tmp/%s_deb/usr/bin/pem" % this_lib
	shutil.copy("./epem/pem.py", path)
	os.chmod(path, 0775)

except Exception,e:
	print "*** ERROR [%s] ***" % e


	
# RELEASING
#
#  The 'deb' command is assumed to have been performed.
#  The 'deb' package is assumed to be sitting in /tmp
#
# =========

# extract "version"
version = read_version()
print "scons: RELEASING version %s" % version

name = "%s_%s-1_i386.deb" % (this_lib, version)
path = "/tmp/%s" % name
print "scons: renaming debian package: %s" % name
shutil.copy('/tmp/%s_deb.deb' % this_lib, path)

print "scons: copying [%s] to repo in dists/main/binary-i386" % path
shutil.copy(path, "../../../dists/stable/main/binary-i386")

debian_path = "../../../dists/stable/main/binary-i386/%s" % name
print "scons: running dpkg-scanpackages  [%s]" % debian_path
os.system("cd ../../.. && dpkg-scanpackages -m dists/stable/main/binary-i386 /dev/null | gzip -9c > dists/stable/main/binary-i386/Packages.gz")

print "scons: removing sources archive build directory"
try:    shutil.rmtree("/tmp/%s/%s" % (this_lib, this_lib))
except: pass

print "scons: creating sources archive"
safe_copytree('.', '/tmp/%s/%s' % (this_lib,this_lib), skip_dirs=['.svn', 'debug', 'release'], dir_mode=0775, make_dirs=True)

print "scons: removing unecessary files"
os.system("rm /tmp/%s/%s/.sconsign.dblite" % (this_lib, this_lib))
os.system("rm -r /tmp/%s/%s/*.pyc" % (this_lib, this_lib))

print "scons: creating ZIP archive"
os.system("cd /tmp/%s && zip -r /tmp/%s-sources-%s.zip %s -x *.svn* *.os *.so *.LOG *.DAT *.settings* *.cproject* *.project* *.pydevproject* *old* *.prefs" % (this_lib, this_lib, version, this_lib))


# UPLOADING


if 'up' in COMMAND_LINE_TARGETS:
	import gcupload as gc
	
	version = read_version()

	print "scons: uploading to Google Code"
	user = get_gcuser()
	pwd  =  get_gcpwd()
	
	#upload(file, project_name, user_name, password, summary, labels=None)
	src = "/tmp/%s-sources-%s.zip" % (this_lib, version)
	gc.upload(src, "phidget-erlang-manager", user, pwd, "sources archive", ["sources", "featured"])
	
	deb = "/tmp/%s_%s-1_i386.deb" % (this_lib, version)
	gc.upload(deb, "phidget-erlang-manager", user, pwd, "debian binary-i386", ["Type-Package", "OpSys-Linux", "featured"])
		
