# Rock-Paper-Scissors-Spock-Lizard as a 2-Party Computation problem

[Rock-Paper-Scissors-Spock-Lizard](https://en.wikipedia.org/wiki/Rock%E2%80%93paper%E2%80%93scissors#Additional_weapons) (RPSSL) was originally
developed by Sam Kass and Karen Bryla, and popularized by the sitcom "The Big 
Bang Theory".

This is an implementation that allows a game of RPSSL between two players with
no need for a trusted third party.

To achieve this goal, we describe the game as a boolean circuit and we use a
garbled circuit (GC) protocol to execute it without revealing the players'
moves.

## Tools

* Python 3 with the [MyHDL](http://www.myhdl.org/) library
* [Yosys Open Synthesis Suite](http://www.clifford.at/yosys/)
* [TinyGarble](https://github.com/esonghori/TinyGarble)
* [JustGarble](http://cseweb.ucsd.edu/groups/justgarble/) (used by TinyGarble as the garbling backend)

## Quick walkthrough

    $ python3 rpssl.py
    $ yosys script.ys
    $ /path/to/TinyGarble/bin/scd/V2SCD_Main -i /path/to/rpssl_netlist.v -o rpssl.scd --log2std

Explanation:

1. Generates `top.v` (behavioral Verilog description of the circuit)
2. Generates `rpssl_netlist.v` (netlist)
3. Turns the netlist into the SCD format used by JustGarble/TinyGarble

Notice that TinyGarlble needs **both** the `scd` and the `scd.map` files
generated at step 3!

An example game:

    # Alice (the garbler, i.e. Player one) plays "Rock"
    $ bin/garbled_circuit/TinyGarble --alice --scd_file rpssl.scd --input 0
    # On another terminal, Bob (the evaluator) plays "Scissors"
    $ bin/garbled_circuit/TinyGarble --bob --scd_file rpssl.scd --input 2
    01 # Alice wins

## Miscellanea

The Yosys-generated netlist is available on EDA Playground: 
<https://www.edaplayground.com/x/6DZS>.

A more in-depth report is [here](https://github.com/lou1306/gssi/blob/master/2pc/report/report.pdf).

## References

[1] V. Kolesnikov and T. Schneider, “Improved Garbled Circuit: Free XOR Gates and Applications,” in Automata, Languages and Programming, Berlin, Heidelberg: Springer Berlin Heidelberg, 2008.

[2] D. Malkhi, N. Nisan, B. Pinkas, and Y. Sella, “Fairplay - Secure Two-Party Computation System,” {USENIX} Secur. Symp., pp. 287–302, 2004.

[3] A. C. Yao, “Protocols for secure computations,” in 23rd Annual Symposium on Foundations of Computer Science (sfcs 1982), 1982, pp. 160–164.

[4] D. Goetsch and J. Glickman, “The Lizard–Spock Expansion,” The Big Bang Theory. CBS, 2008.

[5] E. M. Songhori, S. U. Hussain, A.-R. Sadeghi, T. Schneider, and F. Koushanfar, “TinyGarble: Highly Compressed and Scalable Sequential Garbled Circuits,” in 2015 IEEE Symposium on Security and Privacy, 2015, vol. 2, pp. 411–428.

[6] M. Bellare, V. T. Hoang, and P. Rogaway, “Foundations of garbled circuits,” in Proceedings of the 2012 ACM conference on Computer and communications security - CCS ’12, 2012, p. 784.

[7] M. Bellare, Viet Tung Hoang, S. Keelveedhi, and P. Rogaway, “Efficient Garbling from a Fixed-Key Blockcipher,” in 2013 IEEE Symposium on Security and Privacy, 2013, pp. 478–492.