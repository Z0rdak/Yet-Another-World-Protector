package de.z0rdak.yawp.api.events.region;

import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraftforge.eventbus.api.Event;

import javax.annotation.Nullable;

/**
 * Represents the result of a flag check.
 * Contains the responsible region, the flag, the position, the player and the result.
 */
public class FlagCheckResult extends Event {

    private final FlagCheckEvent flagCheck;

    @Nullable
    private final IProtectedRegion responsibleRegion;

    @Nullable
    private final IFlag flag;

    private FlagState result;

    public FlagCheckResult(FlagCheckEvent flagCheck, FlagState state, @Nullable IProtectedRegion responsibleRegion, @Nullable IFlag flag) {
        this.flagCheck = flagCheck;
        this.responsibleRegion = responsibleRegion;
        this.result = state;
        this.flag = flag;
    }

    @Nullable
    public IProtectedRegion getResponsible() {
        return this.responsibleRegion;
    }

    public FlagCheckEvent getFlagCheck() {
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
