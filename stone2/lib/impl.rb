module Stone
  module Opcodes
	  register :lda, 8
    def lda() rA_load @V end

	  register :ldx, 15
    def ldx() rX_load @V end

	  for i in 1..6 do
      register "ld#{i}", 8+i
      eval("def ld#{i}() rI_load(#{i},@V) end")
    end

	  register :ldan, 16
    def ldan() rA_load -@V end

	  register :ldxn, 23
    def ldxn() rX_load -@V end

    for i in 1..6 do
	    register "ld#{i}n", 16+i
      eval("def ld#{i}() rI_load(#{i},-@V) end")
    end

	  #
	  # Storing operators
  	#
  	register :sta, 24
    def sta() store(@M,@rA,@F) end

  	register :stx, 31
    def stx() store(@M,@rX,@F) end

  	for i in 1..6 do
      register "st#{i}", 24+i
      eval("def st#{i}() store(@M,@rI[#{i}],@F) end")
    end

  	register :stj, 32, :field => 2
    def stj() store(@M,@rJ,@F) end

  	register :stz, 33
    def stz() store(@M,Word.new) end
  
  	#
  	# Arithmetic operators
  	#
  	register :add,1
  	register :sub,2
  	register :mul,3, :time=>10
  	register :div,4, :time=>12
  
  	#
  	# Address transfer operators
  	#
  	register :enta, 48, :field => 2, :time => 1
    def enta
  		rA_load(Word.new(@M,((@M == 0) and (not @CONTENTS.sign))))
    end

  	register :entx, 55, :field => 2, :time => 1
    def entx
  		rX_load(Word.new(@M,((@M == 0) and (not @CONTENTS.sign))))
    end

    for i in 1..6 do
      register "ent#{i}", 48+i, :field => 2, :time => 1
      eval("def ent#{i}() \
  		  rI_load(#{i},Word.new(@M,((@M == 0) and (not @CONTENTS.sign)))) \
        end")
    end

  	register :enna, 48, :field => 3, :time => 1

  	register :ennx, 55, :field => 3, :time => 1

  	for i in 1..6 do
      register "enn#{i}", 48+i, :field => 3, :time => 1
    end

  	register :inca, 48, :field => 0, :time => 1

  	register :incx, 55, :field => 0, :time => 1

  	for i in 1..6 do
      register "inc#{i}", 48+i, :field => 0, :time => 1
    end

  	register :deca, 48, :field => 1, :time => 1

  	register :decx, 55, :field => 1, :time => 1

    for i in 1..6 do
      register "dec#{i}", 48+i, :field => 1, :time => 1
      eval("def dec#{i}() \
  		  rI_load(#{i},@rI[#{i}]-@M) \
        end")
    end
  
  	#
  	# Comparison operators
  	#
  	register :cmpa, 56
    def cmpa() rC_set (@rA[@F/8..@F%8] <=> @V) end

  	register :cmpx, 63

    for i in 1..6 do
  	  register "cmp#{i}", 56+i
    end
  
  	#
  	# Jump operators
  	#
  	register :jmp, 39, :field => 0, :time => 1
    def jmp() rJ_load @loc; @loc = @M end

  	register :jsj, 39, :field => 1, :time => 1

  	register :jov, 39, :field => 2, :time => 1

  	register :jnov, 39, :field => 3, :time => 1

  	{:jl=>[4,"<"],:je=>[5,"=="],:jg=>[6,">"],:jge=>[7,">="],:jne=>[8,"!="],:jle=>[9,"<="]}.each {
  		|key,value|
      register key, 39, :field => value[0], :time => 1
      eval("def #{key}()\
        if @rC #{value[1]} 0 then rJ_load @loc; @loc = @M end end")
  	}

  	[:jan,:jaz,:jap,:jann,:janz,:janp].each_with_index {
  		|key,idx| register key, 40, :field => idx, :time => 1
  	}

  	[:jxn,:jxz,:jxp,:jxnn,:jxnz,:jxnp].each_with_index { 
  		|key,idx| register key, 47, :field => idx, :time => 1
  	}

  	[["jin","<"],["jiz","=="],["jip",">"],["jinn",">="],["jinz","!="],["jinp","<="]].each_with_index {
  		|key,idx|
      for i in 1..6 do
        fname = key[0].sub("i",i.to_s)
  			register fname, 40+i, :field => idx, :time => 1
        eval("def #{fname}() \
          if rI_fetch(#{i}) #{key[1]} 0 then rJ_load @loc; @loc = @M end end")
      end
  	}
  
  	#
  	# Miscellaneous operators
  	#
  	[:sla,:sra,:slax,:srax,:slc,:src].each_with_index {
  		|key,idx|
  		register key, 6, :field => idx
  	}

  	register :move, 7, :field => 1

  	register :nop, 0
    def nop() end

  	register :hlt, 5, :field=>2, :time => 10
    def hlt() halt end

  	register :in, 36, :field => 0, :time => 1
  	register :out, 37, :field => 0, :time => 1
  	register :ioc, 35, :field => 0
  	register :jred, 38, :field => 0, :time => 1
  	register :jbus, 34, :field => 0
  	register :num, 5, :field => 0, :time => 10
  	register :char, 5, :field => 1, :time => 10
  
    compute_dispatch_table
  end
end
