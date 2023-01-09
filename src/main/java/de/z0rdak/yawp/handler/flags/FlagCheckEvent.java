package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.world.entity.player.Player;
import net.minecraftforge.eventbus.api.Cancelable;
import net.minecraftforge.eventbus.api.Event;

import javax.annotation.Nullable;

// TODO: FlagCheck for 'nested' flags (when boolean flag works with true and false) like secondary tool or use portal
@Cancelable
public class FlagCheckEvent extends Event {

    private final DimensionalRegion dimRegion;
    @Nullable
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

    @Cancelable
    public static class PlayerFlagEvent extends FlagCheckEvent {

        private final Player player;

        public PlayerFlagEvent(Player player, DimensionalRegion dimRegion, IMarkableRegion localRegion, RegionFlag flag) {
            super(dimRegion, localRegion, flag);
            this.player = player;
        }

        public Player getPlayer() {
            return player;
        }
    }
}
