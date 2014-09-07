package org.waman.multiverse.javaverse;

import java.math.BigDecimal;

import static org.waman.multiverse.javaverse.Util.dec;

public class NewtonCooling0 {

    public static void main(String... args){

        for(CoolSystem0 s = new CoolSystem0(dec(0.0), 82.3);
                s.T > 50.0;
                s.next()){
            System.out.println(s);
        }
    }
}

class CoolSystem0 {

    final double r = 0.03;
    final double T_room = 17;
    final BigDecimal dt = dec(0.1);

    BigDecimal t;
    double T;

    CoolSystem0(BigDecimal t, double T){
        this.t = t;
        this.T = T;
    }

    void next(){
        BigDecimal _t = t.add(dt);
        double _T = T - r*(T - T_room)*dt.doubleValue();

        this.t = _t;
        this.T = _T;
    }

    @Override
    public String toString(){
        return String.format("%f : %f", t, T);
    }
}