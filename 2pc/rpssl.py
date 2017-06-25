from myhdl import Signal, delay, Simulation, always_comb, always, instance, intbv, bin, toVerilog
from collections import defaultdict
from itertools import product


ROCK, PAPER, SCISSORS, SPOCK, LIZARD = range(5)

def win(X, Y, OUT):
    """Lookup table for the RPSSL game.
    
    Args:
        X (intbv): A RPSSL strategy encoded as a 3-bit integer 
        Y (intbv): A RPSSL strategy encoded as a 3-bit integer
        OUT (TYPE): 1 iff X wins against Y, 0 otherwise
    """
    @always_comb
    def logic():
        if X == ROCK and (Y == SCISSORS or Y == LIZARD): OUT.next = 1
        elif X == PAPER and (Y == ROCK or Y == SPOCK): OUT.next = 1
        elif X == SCISSORS and (Y == PAPER or Y == LIZARD): OUT.next = 1
        elif X == SPOCK and (Y == SCISSORS or Y == ROCK): OUT.next = 1
        elif X == LIZARD and (Y == PAPER or Y == SPOCK): OUT.next = 1
        else: OUT.next = 0
    return logic

def rpssl(X, Y, XWIN, OUT):
    """Combinatorial logic of the RPSSL game
    
    Args:
        X: RPSSL strategy of Player 1 encoded as a 3-bit integer 
        Y: RPSSL strategy of Player 2 encoded as a 3-bit integer 
        XWIN: Output of a "win" block (one bit)
        OUT: 0b00 if a draw; 0b01 = Player 1 wins; 0b11 = Player 2 wins;
        0b10 = invalid input. Notice that OUT = Payoff for Player one as a
        signed 2-bit integer (negative zero = error).
    
    """
    @always_comb
    def logic():
        if X > 4: 
            OUT.next = 0b10
        elif Y > 4: 
            OUT.next = 0b10
        elif X == Y: 
            OUT.next = 0b00
        elif XWIN:
            OUT.next = 0b01
        else: 
            OUT.next = 0b11
    return logic

def top(clk, rst, g_input, e_input, o):
    """The main element in the hierarchy

    The top block instantiates both the `win` and the `rpssl` blocks and wires
    them together.
    """
    OUT_win = Signal(intbv(0)[1:])
    win_instance = win(g_input, e_input, OUT_win)
    rpssl_instance = rpssl(g_input, e_input, OUT_win, o)
    return win_instance, rpssl_instance

def main():
    """Simulation function

    We verify the correctness of our circuit by checking all input combinations.
    """
    X = Signal(intbv(0))
    Y = Signal(intbv(0))
    OUT = Signal(intbv(0))
    clk = Signal(intbv(0))
    rst = Signal(intbv(0))
    dut = top(clk, rst, X, Y, OUT)

    # This is just to pretty-print results
    _encoding = ["ROCK", "PAPER", "SCISSORS", "SPOCK", "LIZARD"]
    ENCODING = defaultdict(lambda: "INVALID")
    ENCODING.update(enumerate(_encoding))

    @instance
    def stimulus():
        print("{:>8}\t{:>8}\t{}".format("X", "Y", "OUT"))
        for x_next, y_next in product(range(8), range(8)):
            X.next = intbv(x_next)
            Y.next = intbv(y_next)
            yield delay(10)
            print(
                "{:>8}".format(ENCODING[int(X)]),
                "{:>8}".format(ENCODING[int(Y)]),
                "{:>3}".format(bin(OUT, 2)),
                sep="\t")
    return dut, stimulus

# Verification phase
sim = Simulation(main())
sim.run()

# Verilog generation phase
g_input = Signal(intbv(0)[3:])
e_input = Signal(intbv(0)[3:])
clk = Signal(intbv(0)[1:])
rst = Signal(intbv(0)[1:])
o = Signal(intbv(0)[2:])
toVerilog(top, clk, rst, g_input, e_input, o)
