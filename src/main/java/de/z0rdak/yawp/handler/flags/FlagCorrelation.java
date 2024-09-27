package de.z0rdak.yawp.handler.flags;


import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import org.jetbrains.annotations.Nullable;


/**
 * A correlation of a region and a flag. <br>
 * This is used to determine the responsible region for a flag state.
 * This region is not necessarily the region responsible for the flag check event.
 * It could be a parent region which overrides the flag state of the child region.
 */
public final class FlagCorrelation {

    public final IProtectedRegion region;
    @Nullable
    public final IFlag flag;

    public FlagCorrelation(IProtectedRegion region, @Nullable IFlag flag) {
        this.region = region;
        this.flag = flag;
    }

    public IProtectedRegion getRegion() {
        return region;
    }

    @Nullable
    public IFlag getFlag() {
        return flag;
    }

    public boolean hasFlag() {
        return flag != null;
    }
}