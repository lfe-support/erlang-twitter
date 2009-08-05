"""
erlang-twitter

@author: Jean-Lou Dupont
"""
import os
import shutil

from helpers import *

this_lib="twitter"

all = [
		{'src': './project/src',     'dst':'./project/packages/debian/usr/lib/erlang/lib/twitter-%s/src'},	   
		{'src': './project/ebin',    'dst':'./project/packages/debian/usr/lib/erlang/lib/twitter-%s/ebin'},
		{'src': './project/include', 'dst':'./project/packages/debian/usr/lib/erlang/lib/twitter-%s/include'},
		]

def release():
	
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
	
	
	except Exception,e:
		print "*** ERROR [%s] ***" % e


def main():
	release()

	
if __name__=="__main__":
	main()