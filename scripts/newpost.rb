#!/usr/bin/env ruby

unless ARGV[0]
  puts "Usage: newpost post-title"
  exit(-1)
end

date_prefix = Time.now.strftime("%Y-%m-%d")
postname = ARGV[0].strip.downcase.gsub(/ /, '-').gsub('(', '-').gsub(')', '-')

header = <<-END
---
layout: post
title: "#{ARGV[0]}"
draft: true
tags:
    - tag 1
    - tag 2
author: crodjer
---

END

f = File.open("/home/rohan/workspace/site/_posts/#{date_prefix}-#{postname}.mkd", 'w+')
f << header
f.close

system("e #{f.path}")
exit(0)
