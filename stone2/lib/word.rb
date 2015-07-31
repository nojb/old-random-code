module Stone
  class Word
    ZERO = self.new

    def initialize(n=0,s=false)
      @contents = n.abs
      @sign     = s ? s : n < 0
    end

    def to_i
      @sign ? -@contents : @contents
    end

    def -@ #unary minus
      Word.new(@contents,(not @sign))
    end

    def to_wval
      "%s%d(1),%d(2),%d(3),%d(4),%d(5)" % [@sign?"-1(0),":"",self[1],self[2],self[3],self[4],self[5]]
    end
  
    def slice(i, j)
      if not (i <= j and 0 <= i and j <= 5) then
        puts "slice: invalid spec"
        return Word.new
      end
  
      k     = ([i,1].max)-1
      mask  = 0b11_1111_1111_1111_1111_1111_1111_1111
      mask  = (mask << (5-j)*6) & mask # first & mask inecessary
      mask  = ((mask << k*6) & mask) >> k*6
  
      r = (@contents & mask) >> (5-j)*6
  
      return Word.new(r, i == 0 ? @sign : false)
    end
  
    def <=>(w)
      if w.kind_of? Word then
        return self.to_i <=> w.to_i
      else
        return self.to_i <=> w
      end
    end
  
    def setfield(i, j, w)
      if not (i <= j and 0 <= i and j <= 5) then
        puts "setfield: invalid spec"
        return self
      end
  
      k         = ([i,1].max)-1
      mask      = 0b11_1111_1111_1111_1111_1111_1111_1111
      mask      = (mask << (5-j)*6) & mask # first & mask innecessary
      mask      = ((mask << k*6) & mask) >> k*6
      n         = w.to_i
      @contents = (@contents & ~mask) | ((n.abs << (5-j)*6) & mask)
      @sign     = (i == 0) ? w.sign : @sign
  
      return self
    end
    private :setfield
  
    def sign
      @sign
    end
  
    def to_s(as_inst=false)
      if not as_inst then
        "%s %02d %02d %02d %02d %02d"% [@sign?"-":"+",self[1],self[2],self[3],self[4],self[5]]
      else
        "%s %04d %02d %02d %02d"% [@sign?"-":"+",self[1..2],self[3],self[4],self[5]]
      end
    end
  
    def +(w)
      Word.new(to_i+w.to_i)
    end
  
    def -(w)
      Word.new(to_i-w.to_i)
    end
  
    def [](n)
      if n.kind_of? Range then
        slice(n.first,n.last).to_i
      else
        slice(n,n).to_i
      end
    end
  
    def []=(n,w)
      if w.kind_of? Integer then
        w = Word.new(w)
      end
      if n.kind_of? Range then
        setfield(n.first,n.last,w)
      else
        setfield(n,n,w)
      end
    end
  
    def !=(w)
      to_i != w.to_i
    end
  
    def >(w)
      to_i > w.to_i
    end
  
    def <=(w)
      to_i <= w.to_i
    end
  
    def <=>(w)
      to_i <=> w.to_i
    end
  
    def >=(w)
      to_i >= w.to_i
    end
  
    def ==(w)
      to_i == w.to_i
    end
  
    alias :inspect :to_s
  end
end
