#!/usr/local/bin/ruby

require "open3"

begin
#  require "rb-fsevent"
  require 'fssm'
  def addMonitor(a,b)
    FSSM.monitor(a,b) do
      create(&Object.method(:notify))
      update(&Object.method(:notify))
      delete(&Object.method(:notify))
    end
  end
rescue LoadError
  def addMonitor(a,b)
    printf "start poling monitor\n"
    olds = nil
    while(true)
      news = {}
      Dir.glob(a+b).each do |file|
        news[file] = File.mtime(file)
      end

      olds = news if olds == nil

      news.each do |file,data|
        if !(olds.include? file) || olds[file] != data then
          notify(a.gsub(/\/$/,""),file.gsub(Regexp.new("^"+a),""))
          break
        end
      end
      olds = news
      sleep(0.3)
    end
  end
end

def notify(a,b)
  puts "\n========================"
  p a
  p b
  test()
end

class Pipes
  def initialize(cmd)
    @pipes = Open3.popen3(cmd)
  end

  def self.exec(cmd)
    Open3.capture3(cmd)[0]
  end

  def ppread()
    rc = ""
    while (buffer = @pipes[1].readchar) 
      rc += buffer
      if rc =~ /^# $/m then
        break
      end
    end
    rc.gsub(/\n?# $/m, "")
  end

  def ppwrite(data)
    @pipes[0].print data
    @pipes[0].flush
  end

  def [](n)
    @pipes[n]
  end
end

def debug
  ARGV.include? "-d"
end

def tab
  if ARGV.include? "-notab" then "    " else "\t" end
end

def addLineNo(src)

  line = 0
  src.gsub(/^/) {
    line += 1
    sprintf("%d: ", line)
  }.gsub(/(\(\*\|)(.*?)(\*\))/m){
    m = [$1,$2,$3]
    m[1].gsub!(/(;;[ \t]*\n)([0-9]+:[ \t]*>>>[^;]*)/) {
      $1 + "00: " + tab + "\n" + $2
    }
    m.join
  }
end

def getMDSrc(src)

  src.scan(/\(\*\|(.*?)\*\)/mu).join.gsub(/\A\s+/, "")
end

def getTests(src)
  getMDSrc(src).scan(
    /^([0-9]+):[ \t]*>>>[ \t]*(.*?;;)[ \t]*\n[0-9]+:[ \t]*([^\n]*?)[ \t]*\n/m
  ).map{|m|
    [m[0],{:test=>m[1].gsub(/^[0-9]+:[ \t]*/, ""), :expected=>m[2]}]
  }.to_h
end

def getRewriteMLs(mls)
  ARGV.each do |arg|
    if (m = arg.match(/^--rewrite=(.*)$/)) then
      ws = m[1].split(",")
      if ws.include? "all" then
        return mls
      else
        return ws.map{|w| w + ".ml"}
      end
    end
  end
  []
end

def rewriteML(ml, src, tests)
  puts "# rewrite "+ml+"."

  src.gsub!(/(\(\*\|)(.*?)(\*\))/m){
    m1 = [$1,$2,$3]
    m1[1].gsub!(/^([0-9]+)(:[ \t]*>>>[ \t]*.*?;;[ \t]*\n[0-9]+:)([ \t]*)([^\n]*?)([ \t]*\n)/m) {|mm|
      m = [$1,$2,$3,$4,$5]
      m[3] = tests[m[0]][:expected]
      if m[2] !~ Regexp.new("\t|"+tab) then
        m[2] = " " + tab
        m[3] += "\n" if m[3] != ""
      end
      m.join
    }
    m1.join
  }
  File.write(ml + ".bak", File.read(ml))
  File.write(ml, src.gsub(/^[0-9]+: /, ""))
end

def testMLs(pipes, mls)

  allcount = allok = 0

  mls.each do |ml|
    count = ok = 0

    puts "# start " + ml + " test"

    src = addLineNo(File.read(ml))
    tests = getTests(src)
    tests.each do |line,test|

      pipes.ppwrite(test[:test]+"\n")
      result = pipes.ppread.gsub(/[\n\t ]+/m, " ").gsub(/^\s+/m, "")
      if debug then
        printf "# %d %s\n", count, test[:test]
        puts result
      end

      count += 1
      if result == test[:expected] then
        ok += 1
        next
      end

      puts "fail(" + line + "):"
      if test[:expected] == "" then
        puts "\t"+result
      else
        puts "expcted: " + test[:expected]
        puts "but found:\n" + tab + result
      end
      test[:expected] = result
    end

    rewriteML(ml, src, tests) if count != ok && getRewriteMLs(mls).include?(ml)

    printf("test %d ok %d ng %d\n", count, ok, (count - ok))

    allcount += count
    allok += ok
  end

  printf("all test %d ok %d ng %d\n", allcount, allok, allcount - allok)
end

def test() 

  def getMLs

    mls = ARGV.select{|ml| ml =~ /.*\.ml$/}

    mls = Pipes.exec("ocamldep " + mls.join(" ")).scan(/([^ ]+)\.cmo/).map {|m|
      m[0] + ".ml"
    }

    Pipes.exec("ocamldep -sort " + mls.join(" ")).split(/ +/).select{|ml| ml =~ /\.ml$/ }
  end
  mls = getMLs

  Pipes.exec("ocamlc "+mls.join(" "))

  pipes = Pipes.new("ocaml")

  pipes.ppread

  mls.each {|ml|
    cmo = ml.gsub(/\.ml$/, ".cmo")
    pipes.ppwrite("#load \""+cmo+"\";;\n")
    pipes.ppread
  }

  testMLs(pipes, mls)

  pipes.ppwrite("exit 0;;\n")
  pipes[0].close
  pipes[3].value.exitstatus

  def odoc()

    return if !ARGV.include? "-odoc"

    getMLs.each do |ml|
      md = ml.gsub(/\.ml$/, ".md")
      puts "generate " + md
      File.write(md, getMDSrc(File.read(ml)))
    end
  end
  odoc()

end

def main()
  test()
  addMonitor(Dir::getwd+"/",'**/*.ml') if ARGV.include? "-P"
end

main()