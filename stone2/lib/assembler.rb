require 'polyglot'
require 'treetop'
require 'word'
require 'expr'
require 'wval'
require 'addr'
require 'opcodes'

module Stone
  class Assembler
    class End < Exception
    end
    class Undef < Exception
    end

    def initialize(fname)
      puts "Opening file: #{fname}"
      @code = File.readlines(fname).map { |u| u.chomp }
    end

    def start
      @loc = 0
      @constants = []
      @env = {}
      @undefined = []
      @orig = nil

      @code = @code.map do
        |x|
        if x =~ /^([0-9]*[a-zA-Z]+[a-zA-Z0-9]*)?\s+([a-zA-Z][0-9a-zA-Z]*)(?:\s+([^;]*))?(?:;(.*))?/ then
          {:label=>$1,:op=>$2.downcase,:address=>$3 ? $3.strip : "",:text=>x}
        else
          {:label=>"",:op=>nil,:address=>"",:text=>x}
        end
      end

      begin
        @code.each do
          |ln|
          if (x = preprocess ln) then
            ln[:should_assemble] = true
            ln[:loc] = x
          else
            ln[:should_assemble] = false
          end
        end
      rescue End
        @code.values_at(@loc..@code.size-1).each do
          |x|
          x[:should_assemble] = false
        end
      end

      if not @orig then
        puts "warning, there was no END instruction, setting ORIG to 0"
        @orig = 0
        # add end instruction
      end

      puts "------ Environtment follows --------".upcase
      puts @env
      puts "---- ASSEMBLING ----"

      @code.each do
        |x|
        if x[:should_assemble] then
          puts "about to asm [#{x[:op]}\t\t#{x[:address]}] at #{x[:loc]}"
          x[:code] = assemble x
          puts "assembled instruction #{x[:code].to_s(true)} at #{x[:loc]}"
        end
      end

      @constants.each_with_index do
        |c,idx|
        puts "assembling constant #{c} at #{@loc}"
        @code.insert(@loc, {
          :loc              => @loc,
          :code             => c,
          :text             => "__CONST#{idx}\tcon\t#{c.to_wval}",
          :should_assemble  => true
        })
        @loc += 1
      end

      @undefined.each do
        |s|
        puts "defining undefined symbol #{s} at #{@loc}"
        @code.insert(@loc, {
          :loc              => @loc,
          :code             => Word.new,
          :text             => "#{s}\tequ\t0",
          :should_assemble  => false
        })
        @loc += 1
      end

      return true
    end

    def preprocess(ln)
      puts "preprocessing :op => #{ln[:op]}, :address => #{ln[:address]}"
      case ln[:op]
      when nil then
        return nil
      when "equ" then
        define_label(ln[:label],eval_wval(ln[:address]))
        return nil
      when "orig" then
        define_label(ln[:label])
        @loc = eval_wval(ln[:address]).to_i
        return nil
      when "con" then
        @loc += 1
        return (@loc-1)
      when "alf" then
        puts "warning: not handling alf"
        @loc += 1
        return (@loc-1)
      when "end" then
        @orig = eval_wval(ln[:address])[4..5]
        raise End
      else # an actual mix opcode
        define_label(ln[:label])
        @loc += 1
        return (@loc-1)
      end
    end

    def define_label(s,v=nil)
      return nil unless (s and s!="")
      if s =~ /^([0-9])[hH]$/ then
        if not @env[$1] then
          @env[$1] = []
        end
        @env[$1].push(v ? v.to_i : @loc)
      else
        @env[s] = v ? v.to_i : @loc
      end
    end
  
    def define_constant(v)
      @constants << v
      return (@loc+@constants.size-1)
    end
  
    def eval_wval(s)
      puts "Evaluating wval: '#{s}'"
      result = WvalParser.new.parse(s).eval(self)
      puts "\t#{result} (#{result.to_i})"
      return result
    end
  
    def loc
      @loc
    end

    def orig
      @orig
    end
  
    def lookup_label(s,raise_if_undef=false)
      if s and s != "" then
        if s =~ /^([0-9])[bB]$/ then
          return @env[$1].last
        elsif s =~ /^[0-9][hHfF]$/ then
          puts "warning: here or forward references illegal. setting to zero"
          return 0
        elsif (x = @env[s]) then
          return x
        else
          puts "warning: #{s} not defined, setting to zero"
          @undefined << s
          if raise_if_undef then raise Undef
          else return 0
        end
      else
        puts "this shouldn't happen: lookup_label was called with an empty or nil string"
        return nil # shouldn't happen
      end
    end
  
    def assemble(ln)
      case ln[:op]
      when "alf" then
        puts "warning: ignoring alf"
        return Word.new
      when "con" then
        return eval_wval(ln[:address])
      else #an actual mix opcode
        addrParser = AddrParser.new
        x = addrParser.parse(ln[:address]).asm(self)
        puts "parsed addr part of insturction to be #{x}"
        x[:field] ||= Opcodes.op_field(ln[:op])
        code = Word.new
        code[0..2]  = x[:addr]
        code[3]     = x[:index]
        code[4]     = x[:field]
        code[5]     = Opcodes.op_code(ln[:op])
        return code
      end
    end

    def load(vm)
      puts "in load()"
      puts @code
      @code.each do |ln|
        if ln[:should_assemble] then
          vm.store(ln[:loc],ln[:code])
        end
      end
      vm.orig= @orig
    end
  
    def inspect
      "A parser for MIX files"
    end
  end
end
