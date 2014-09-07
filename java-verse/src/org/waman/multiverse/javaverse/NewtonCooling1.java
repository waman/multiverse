package org.waman.multiverse.javaverse;

import java.math.BigDecimal;
import java.util.Optional;
import java.util.stream.Stream;

import static org.waman.multiverse.javaverse.Util.dec;

public class NewtonCooling1 {

    public static void main(String... args){
        CoolSystem1 system = new CoolSystem1(dec(0.0), 82.3);
        Stream<CoolSystem1> stream = Stream.iterate(system, CoolSystem1::next);

        Optional<CoolSystem1> result = stream.peek(System.out::println)
                .filter(s -> s.T < 50.0)
                .findAny();

        System.out.println();
        System.out.println(result.get());
    }
}

class CoolSystem1 {

    static final double r = 0.03;
    static final double T_room = 17;
    static final BigDecimal dt = dec(0.1);

    final BigDecimal t;
    final double T;

    CoolSystem1(BigDecimal t, double T){
        this.t = t;
        this.T = T;
    }

    CoolSystem1 next(){
        BigDecimal _t = t.add(dt);
        double _T = T - r*(T - T_room)*dt.doubleValue();
        return new CoolSystem1(_t, _T);
    }

    @Override
    public String toString(){
        return String.format("%f : %f", t, T);
    }
}