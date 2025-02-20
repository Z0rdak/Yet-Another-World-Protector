package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.core.region.IProtectedRegion;
import org.jetbrains.annotations.Nullable;

/**
 * Represents the resolved association between a region and a flag. <br>
 * This is used to determine the region responsible for a specific flag state. <br>
 * The responsible region may not always be the one directly involved in a flag check event;
 * it could be a parent region that overrides the flag state of a child region. <br>
 * <br>
 * This class is primarily used for passing information between recursive calls when evaluating flag checks
 * and for retaining flag resolution details for use in the CLI.
 */
public record FlagCorrelation(
        IProtectedRegion region,
        @Nullable IFlag flag) {
}
