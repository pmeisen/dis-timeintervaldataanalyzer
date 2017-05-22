package net.meisen.master.meike.correctness;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;

public class KuhnMunkresAssert extends AbstractAssert<KuhnMunkresAssert, KuhnMunkresResult> {

    private KuhnMunkresAssert(KuhnMunkresResult actual) {
        super(actual, KuhnMunkresAssert.class);
    }

    public static KuhnMunkresAssert assertThatResult(final KuhnMunkresResult result) {
        return new KuhnMunkresAssert(result);
    }

    public KuhnMunkresAssert hasValidCosts() {
        Assertions.assertThat(actual.plainDistanceResult.getValue())
                .isGreaterThanOrEqualTo(actual.bestShiftDistanceResult.getValue());
        Assertions.assertThat(actual.iterativeShiftDistanceResult.getValue())
                .isGreaterThanOrEqualTo(actual.bestShiftDistanceResult.getValue());
                //.isBetween(actual.bestShiftDistanceResult.getValue(),
                //        actual.plainDistanceResult.getValue());
        return this;
    }
}
