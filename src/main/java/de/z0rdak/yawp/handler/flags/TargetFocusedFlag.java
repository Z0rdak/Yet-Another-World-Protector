package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.core.flag.RegionFlag;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

@Retention(value = RUNTIME)
@Target(value = METHOD)
/**
 * A target related flag, cares about the target itself and not about the source of an event
 * For now, this is just a marker annotation. This may get removed when flags are categorized someday.
 */
public @interface TargetFocusedFlag {
    RegionFlag flag();
}
