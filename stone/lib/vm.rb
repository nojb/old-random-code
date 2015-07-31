require 'word'
require 'opcodes'

module Stone
  class VM
    MEMORY_SIZE = 4000

    include Opcodes

    def initialize
      @memory = Array.new(MEMORY_SIZE) { Word.new }
      @loc = 0
      @halted = true
      @M = 0
      @AA = 0
      @F = 0
      @I = 0
      @rI = Array.new(7,Word.new)
      @rJ = Word.new
      @rA = Word.new
      @rX = Word.new
      @rC = 0 #comparison toggle
      @time = 0
    end
  
    def self.define_reg_accessors(s)
      eval("def r#{s}_load(w)
           @r#{s} = w
           if respond_to? :r#{s}_loaded then r#{s}_loaded w end
           end")
      eval("def r#{s}_fetch()
           if respond_to? :r#{s}_fetched then r#{s}_fetched end
           @r#{s}
           end")
    end
  
    define_reg_accessors(:A)
    define_reg_accessors(:J)
    define_reg_accessors(:X)
  
    def rI_load(i,w)
      @rI[i] = w
      if respond_to? :rI_loaded then rI_loaded(i,w) end
    end
  
    def rI_fetch(i)
      if respond_to? :rI_fetched then rI_fetched(i) end
      @rI[i]
    end
  
    def halted?
      @halted
    end
  
    def halt(b=true)
      @halted = b
      if respond_to? :halted then halted(b) end
    end
  
    def fetch(n)
      x = @memory.fetch(n)
      if respond_to? :fetched then fetched(n,x) end
      return x
    end
  
    def store(n,w,field=5)
      @memory[n][field/8..field%8]=w
      if respond_to? :stored then stored(n,w,f) end
    end
  
    def rC_set(n)
      @rC = n
      if respond_to? :comparison_set then comparison_set n end
    end
  
    def time
      @time
    end

    def orig= x
      @loc = x
    end
  
    def run(orig=nil)
      time = 0
      halt(false)
      @loc = orig || @loc
      while not halted?
        step
      end
      @time
    end
  
    def to_s
      "A Mix Interpreter (LOC = #{@loc})"
    end
  
    alias :inspect :to_s
  
    def add
      v = @rA + @V
      if v != 0 then
        rA_load(v)
      end
    end
  
    def sub
      v = @rA - @V
      if v != 0 then
        rA_load(v)
      end
    end
  end
end
