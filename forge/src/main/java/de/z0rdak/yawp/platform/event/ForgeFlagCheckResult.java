package de.z0rdak.yawp.platform.event;

import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.api.events.flag.FlagCheckResult;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraftforge.eventbus.api.Event;

import javax.annotation.Nullable;

/**
 * Represents the result of a flag check.
 * Contains the responsible region, the flag, the position, the player and the result.
 */
public class ForgeFlagCheckResult extends Event {

    private final ForgeFlagCheckRequest flagCheck;

    @Nullable
    private final IProtectedRegion responsibleRegion;

    @Nullable
    private final IFlag flag;

    private FlagState result;

    public ForgeFlagCheckResult(ForgeFlagCheckRequest flagCheck, FlagState state, @Nullable IProtectedRegion responsibleRegion, @Nullable IFlag flag) {
        this.flagCheck = flagCheck;
        this.responsibleRegion = responsibleRegion;
        this.result = state;
        this.flag = flag;
    }

    public static FlagCheckResult asNonEvent(ForgeFlagCheckResult result) {
        ForgeFlagCheckRequest check = result.flagCheck;
        FlagCheckRequest checkEvent = new FlagCheckRequest(check.getTarget(), check.getRegionFlag(), check.getDimension(), check.getPlayer(), check.getId());
        return new FlagCheckResult(checkEvent, result.result, result.responsibleRegion, result.flag);
    }

    public static ForgeFlagCheckResult asEvent(FlagCheckResult result) {
        FlagCheckRequest check = result.getFlagCheck();
        ForgeFlagCheckRequest checkEvent = new ForgeFlagCheckRequest(check.getTarget(), check.getRegionFlag(), check.getDimension(), check.getPlayer(), check.getId());
        return new ForgeFlagCheckResult(checkEvent, result.getFlagState(), result.getResponsible(), result.getFlag());
    }

    @Nullable
    public IProtectedRegion getResponsible() {
        return this.responsibleRegion;
    }

    public ForgeFlagCheckRequest getFlagCheck() {
        return flagCheck;
    }

    public FlagState getFlagState() {
        return result;
    }

    public void setFlagState(FlagState result) {
        this.result = result;
    }

    @Nullable
    public IFlag getFlag() {
        return this.flag;
    }


}
