(ns aoc2018.day19
  (require [aoc2018.utils :as u]
           [aoc2018.day16 :as d16]))

; --- Day 19: Go With The Flow ---
; "In programs where flow control is required, the instruction pointer can be bound to a register so that it can be manipulated directly. This way, setr/seti can function as absolute jumps, addr/addi can function as relative jumps, and other opcodes can cause truly fascinating effects."
; This mechanism is achieved through a declaration like #ip 1, which would modify register 1 so that accesses to it let the program indirectly access the instruction pointer itself. To compensate for this kind of binding, there are now six registers (numbered 0 through 5); the five not bound to the instruction pointer behave as normal. Otherwise, the same rules apply as the last time you worked with this device.
; When the instruction pointer is bound to a register, its value is written to that register just before each instruction is executed, and the value of that register is written back to the instruction pointer immediately after each instruction finishes execution. Afterward, move to the next instruction by adding one to the instruction pointer, even if the value in the instruction pointer was just updated by an instruction. (Because of this, instructions must effectively set the instruction pointer to the instruction before the one they want executed next.)
; The instruction pointer is 0 during the first instruction, 1 during the second, and so on. If the instruction pointer ever causes the device to attempt to load an instruction outside the instructions defined in the program, the program instead immediately halts. The instruction pointer starts at 0.
; What value is left in register 0 when the background process halts?

(def input (u/resource->strings "day19.txt"))

(defn parse-ip-register [input]
  (->>
      (first input)
      (re-find #"\#ip\s+(\d+)")
      (second)
      (u/s->i)))

(defn parse-program [input]
  (->>
      input
      (rest)
      (map #(re-find #"(\S+)\s+(\d+)\s+(\d+)\s+(\d+)" %))
      (map (fn [[_ op a b c]] [(get d16/ops (keyword op)) [(u/s->i a) (u/s->i b) (u/s->i c)]]))
      (into [])))

(defn run-program [input registers]
  (let [ip-register-idx (parse-ip-register input)
        program (parse-program input)]
    (loop [r registers
           ip-value 0]
      (if (or (neg? ip-value) (>= ip-value (count program)))
          r
          (let [pre-r (assoc r ip-register-idx ip-value)
                op (first (get program ip-value))
                arg (second (get program ip-value))
                post-r (op arg pre-r)
                next-ip-value (inc (get post-r ip-register-idx))]
            (recur post-r next-ip-value))))))
(comment
  (println "Register 0 will contain value" (first (run-program input (into [] (repeat 6 0))))))
; Register 0 will contain value 1836

; --- Part Two ---
; A new background process immediately spins up in its place. It appears identical, but on closer inspection, you notice that this time, register 0 started with the value 1.
; What value is left in register 0 when this new background process halts?

; Used https://github.com/theonejb/advent-of-code-18/blob/master/src/aoc18/day19.clj for disassembling
; program is calculating sum of divisors, but the target number has to be resolved from the code itself
; in my case
;
; 17 B = B + 2    B = 2
; 18 B = B x B    B = 2*2
; 19 B = IP x B   B = 2*2*19
; 20 B = B x 11   B = 2*2*19*11 = 836
; 21 E = E + 7    E = 7
; 22 E = E x IP   E = 7*22
; 23 E = E + 20   E = 7*22+20 = 174
; 24 B = B + E    B = 836+174 = 1010
; 25 IP = IP + A  if (a == 1) goto L2
; 26 IP = 0       else goto L3
; 27 E = IP       E = 27
; 28 E = E x IP   E = 27*28
; 29 E = IP + E   E = 27*28+29
; 30 E = IP x E   E = (27*28+29)*30
; 31 E = E x 14   E = (27*28+29)*30*14
; 32 E = E x IP   E = (27*28+29)*30*14*32
; 33 B = B + E    B = 1010+10550400 = 10551410
;
; then http://www.javascripter.net/math/calculators/divisorscalculator.htm was used to calculate result
; 10551410 => Sum of divisors: 18992556. 8 divisors: 1 2 5 10 1055141 2110282 5275705 10551410