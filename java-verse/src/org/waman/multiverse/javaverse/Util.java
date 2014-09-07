package org.waman.multiverse.javaverse;

import java.math.BigDecimal;
import java.math.MathContext;

class Util{

    static final MathContext MC = new MathContext(1);

    static BigDecimal dec(double d){
        return new BigDecimal(d, MC);
    }
}
