package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
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
    /**
     * The region that is responsible for the flag check.
     */
    private final IProtectedRegion responsibleRegion;
    private final IFlag flag;
    private final RegionFlag regionFlag;
    /**
     * The target position of the flag check result. Depending on the flag this can refer to a block position or a entity position.
     */
    private final BlockPos pos;
    /**
     * The player that triggered the flag check, may be null when no player was involved. This depends on the checked flag.
     */
    @Nullable
    private final Player player;
    private final ResourceKey<Level> dim;
    private FlagState result;

    public FlagCheckResult(RegionFlag flag, FlagState state, BlockPos pos, IProtectedRegion responsibleRegion, @Nullable Player player) {
        this.responsibleRegion = responsibleRegion;
        this.dim = responsibleRegion.getDim();
        this.pos = pos;
        this.player = player;
        this.result = state;
        this.regionFlag = flag;
        this.flag = responsibleRegion.getFlagContainer().get(flag.name);
    }

    public IProtectedRegion getResponsible() {
        return this.responsibleRegion;
    }

    public FlagState getFlagState() {
        return result;
    }

    public void setFlagState(FlagState result) {
        this.result = result;
    }

    public RegionFlag getRegionFlag() {
        return regionFlag;
    }

    public IFlag getFlag() {
        return this.flag;
    }

    public BlockPos getPos() {
        return this.pos;
    }

    @Nullable
    public Player getPlayer() {
        return this.player;
    }

    public ResourceKey<Level> getDim() {
        return this.dim;
    }
}
