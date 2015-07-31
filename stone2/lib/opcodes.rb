require 'word'

module Stone
  module Opcodes
    @@op_dispatch = {}
    @@op_times = {}
    @@op_codes = {}
    @@op_field = {}

    def self.compute_dispatch_table
      values = @@op_codes.values.uniq
      for i in values do
        keys = @@op_codes.select {|key,value| value == i}
        if keys.size == 1 then
          @@op_dispatch[i] = keys.first.first
        else
          @@op_dispatch[i] = "op#{i}".intern
          tbl = {}
          keys.each {|x| tbl[@@op_field[x.first]] = x.first.intern}
          eval("@@op#{i} = #{tbl}")
          eval("def op#{i}() send(@@op#{i}[@F]) end")
        end
      end
    end

    def step
      @CONTENTS = fetch(@loc)
  
      #puts "CONTENTS(LOC = #{@loc}) = #{@CONTENTS.to_s(true)}"
  
      @AA = @CONTENTS[0..2]
      @I  = @CONTENTS[3]
      @F  = @CONTENTS[4]
      @C  = @CONTENTS[5]
      @M  = @AA + @rI[@I].to_i
      @V  = (v = @memory[@M]) ? v.slice(@F/8,@F%8) : nil
  
      puts("Executing #{Opcodes.op_name(@C,@F)} (C = #{@C}, F = #{@F}, I = #{@I}, AA = #{@AA}, M = #{@M}, loc = #{@loc})")
      @loc += 1 # loc points to following instruction during execution
      @time += Opcodes.op_time(@C,@F)
  
      send(@@op_dispatch[@C])
    end

    def self.register(name,code,extra={})
      name = name.intern
      @@op_times[name] = extra[:time] || 2
      @@op_codes[name] = code
      @@op_field[name] = extra[:field] || 5
    end

    #
    # Returns the default field specification
    # for the instruction +name+
    #
    def self.op_field(name)
      @@op_field[name.intern]
    end

    #
    # Return the name of the opcode corresponding
    # to a given (C,F) pair. Returns nil if the pair
    # doesn't correspond to any instruction.
    #
    def self.op_name(c,f)
      begin
        x = class_variable_get("@@op#{c}")
        return x[f].to_s
      rescue NameError
      end
      @@op_dispatch[c].to_s
    end

    def self.op_code(name)
      @@op_codes[name.intern]
    end

    def self.op_time(c,f)
      @@op_times[self.op_name(c,f)] || 0
    end
  end
end

require 'impl'
