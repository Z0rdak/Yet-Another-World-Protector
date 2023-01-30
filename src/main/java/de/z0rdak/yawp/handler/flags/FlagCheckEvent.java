package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.entity.player.PlayerEntity;

// TODO: FlagCheck for 'nested' flags (when boolean flag works with true and false) like secondary tool or use portal
public class FlagCheckEvent {
    private final DimensionalRegion dimRegion;
    private final IMarkableRegion localRegion;
    private final RegionFlag flag;
    private boolean isDenied;
    private boolean isDeniedInDim;
    private boolean isDeniedLocal;

    public FlagCheckEvent(DimensionalRegion dimRegion, IMarkableRegion localRegion, RegionFlag flag) {
        this(dimRegion, localRegion, flag, true, true, true);
    }

    public FlagCheckEvent(DimensionalRegion dimRegion, IMarkableRegion localRegion, RegionFlag flag, boolean isDenied, boolean isDeniedLocal, boolean isDeniedInDim) {
        this.dimRegion = dimRegion;
        this.localRegion = localRegion;
        this.flag = flag;
        this.isDenied = isDenied;
        this.isDeniedLocal = isDeniedLocal;
        this.isDeniedInDim = isDeniedInDim;
    }

    public RegionFlag getFlag() {
        return flag;
    }

    public boolean isDenied() {
        return isDenied;
    }

    public void setDenied(boolean denied) {
        isDenied = denied;
    }

    public boolean isDeniedInDim() {
        return isDeniedInDim;
    }

    public void setDeniedInDim(boolean deniedInDim) {
        isDeniedInDim = deniedInDim;
    }

    public boolean isDeniedLocal() {
        return isDeniedLocal;
    }

    public void setDeniedLocal(boolean deniedLocal) {
        isDeniedLocal = deniedLocal;
    }

    public DimensionalRegion getDimRegion() {
        return dimRegion;
    }

    public IMarkableRegion getLocalRegion() {
        return localRegion;
    }

    public static class PlayerFlagEvent extends FlagCheckEvent {

        private final PlayerEntity player;

        public PlayerFlagEvent(PlayerEntity player, DimensionalRegion dimRegion, IMarkableRegion localRegion, RegionFlag flag) {
            super(dimRegion, localRegion, flag);
            this.player = player;
        }

        public PlayerEntity getPlayer() {
            return player;
        }
    }
}
