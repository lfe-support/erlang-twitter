"""
erlang-twitter

@author: Jean-Lou Dupont
"""
import os
import shutil
import sys
from optparse import OptionParser

from helpers import *

lib="twitter"
this_lib="erlang-twitter"

all = [
		{'src': './project/src',     'dst':"./project/packages/debian/usr/lib/erlang/lib/"+lib+"-%s/src"},	   
		{'src': './project/ebin',    'dst':'./project/packages/debian/usr/lib/erlang/lib/'+lib+'-%s/ebin'},
		##{'src': './project/include', 'dst':'./project/packages/debian/usr/lib/erlang/lib/twitter-%s/include'},
		]

class targets(object):

	@classmethod
	def debian(cls):
		
		print "Preparing .deb package"
		try:
			version = read_version()
			print """> building version [%s]""" % version
			
			print """> cloning files to packages/debian"""
		
			for one in all:
				src = one['src']
				dst = one['dst']
				#release
				print "> cloning [%s]" % src
				safe_copytree(	src , dst % version, skip_dirs=[".svn", "_old"] )
			
			print """> cloning daemon control files to /etc/init.d"""
			path="./project/packages/debian/etc/init.d"
			shutil.copy("./project/"+lib,            path)
			shutil.copy("./project/"+lib+"_control", path)
			
			print """> removing /tmp directory"""
			shutil.rmtree('/tmp/%s_deb' % this_lib, ignore_errors=True)
		
			print """> cloning ./project/packages/debian to /tmp directory"""
			safe_copytree('./project/packages/debian', '/tmp/%s_deb' % this_lib, skip_dirs=['.svn',], dir_mode=0775, make_dirs=True)
		
			print """> adjusting version in control files"""
			c_path = '/tmp/%s_deb/DEBIAN' % this_lib
			params = {'version':version}
			adjust_control_files(params, c_path)
			
			print """> adjusting permissions for `dkpg-deb` command-line"""
			recursive_chmod("/tmp/%s_deb" % this_lib, mode=0775)
		
			#env.Command("deb", "/tmp/%s_deb" % this_lib, "dpkg-deb --build $SOURCE")
			os.system("cd /tmp && dpkg-deb --build /tmp/%s_deb" % this_lib)
		
		except Exception,e:
			print "*** ERROR [%s] ***" % e
	
	@classmethod
	def release(cls):
		# extract "version"
		version = read_version()
		print "> RELEASING version %s" % version
		
		print "> current dir [%s]" % os.getcwd()
		
		name = "%s_%s-1_all.deb" % (this_lib, version)
		path = "/tmp/%s" % name
		print "> renaming debian package: %s" % name
		shutil.copy('/tmp/%s_deb.deb' % this_lib, path)
	
		debian_base="../dists/stable/main/binary-i386"
		print "> copying [%s] to repo in dists/main/binary-i386" % path
		shutil.copy(path, debian_base)

		debian_base="../dists/stable/main/binary-amd64"
		print "> copying [%s] to repo in dists/main/binary-amd64" % path
		shutil.copy(path, debian_base)

		
		debian_path = debian_base + "/" + name
		print "> running dpkg-scanpackages  [%s]" % debian_path
		os.system("cd .. && dpkg-scanpackages -aall -m dists/stable/main/binary-i386 /dev/null | gzip -9c > dists/stable/main/binary-i386/Packages.gz")
		os.system("cd .. && dpkg-scanpackages -aall -m dists/stable/main/binary-amd64 /dev/null | gzip -9c > dists/stable/main/binary-amd64/Packages.gz")

		
		print "> removing sources archive build directory"
		try:    shutil.rmtree("/tmp/%s/%s" % (this_lib, this_lib))
		except: pass
		
		print "> creating sources archive"
		safe_copytree('.', '/tmp/%s/%s' % (this_lib,this_lib), skip_dirs=['.svn', '_old', 'ebin', 'packages'], dir_mode=0775, make_dirs=True)
		
		#print "> removing unecessary files"
		#os.system("rm /tmp/%s/%s/.sconsign.dblite" % (this_lib, this_lib))
		#os.system("rm -r /tmp/%s/%s/*.pyc" % (this_lib, this_lib))
		
		print "> creating ZIP archive"
		os.system("cd /tmp/%s && zip -r /tmp/%s-sources-%s.zip %s -x *.svn* *.os *.so *.LOG *.DAT *.settings* *.cproject* *.project* *.pydevproject* *.pyc *old* *.prefs" % (this_lib, this_lib, version, this_lib))

	def up(cls):
		import gcupload as gc
	
		version = read_version()

		print "> uploading to Google Code"
		try:
			user = get_gcuser()
		except:
			print "! can't find Google Code username [~/.gcuser]\n"
			sys.exit(1)
			
		try:
			pwd  = get_gcpwd()
		except:
			print "! can't find Google Code password [~/.gcpwd]\n"
			sys.exit(1)
			
		
		#upload(file, project_name, user_name, password, summary, labels=None)
		src = "/tmp/%s-sources-%s.zip" % (this_lib, version)
		gc.upload(src, this_lib, user, pwd, "sources archive", ["sources", "featured"])
		
		deb = "/tmp/%s_%s-1_i386.deb" % (this_lib, version)
		gc.upload(deb, this_lib, user, pwd, "debian", ["Type-Package", "OpSys-Linux", "featured"])

	
	def default(cls):
		cls.debian()
		cls.release()


def main():
	usage= """make.py [target]
			
Defaults to 'all' if no 'target' is specified
"""
	
	parser=OptionParser(usage)

	(options, args) = parser.parse_args()
    
	if len(args) > 1:
		parser.error("incorrect number of arguments")
      
	try:    target = args[0]
	except: target = 'default'

	try:
		getattr(targets(), target)()
	except:
		print "! invalid target\n"

	
if __name__=="__main__":
	main()