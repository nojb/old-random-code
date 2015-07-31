class cpu =
    object
        val mutable rA = 0
        val mutable rX = 0
        val mutable rJ = 0
        val rI = Array.make 7 0
        val mem = Array.make 4000 0
        val mutable overflow = 0
        val mutable clock = 0
        val mutable loc = 0
        val mutable halted = false

        method fetch = let x = mem.(loc) in loc <- loc + 1; x

        method step =

        method halt = halted <- true
end
