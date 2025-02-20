package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.core.region.IProtectedRegion;
import org.jetbrains.annotations.Nullable;

/**
 * Represents the resolved association between a region and a flag.
 * <p>
 * This class determines which region is responsible for a specific flag state.  
 * The responsible region may not always be the one directly involved in a flag check event;  
 * it could be an ancestor region that overrides the flag state of a child region.
 * </p>
 * <p>
 * {@code FlagCorrelation} is primarily used for passing information in recursive flag evaluations  
 * and retaining flag resolution details for use in the CLI.
 * </p>
 *
 * @param region the region responsible for the resolved flag state, must not be {@code null}
 * @param flag   the flag associated with the resolved state, may be {@code null} if undefined
 */
public record FlagCorrelation(
        IProtectedRegion region,
        @Nullable IFlag flag) {
}
