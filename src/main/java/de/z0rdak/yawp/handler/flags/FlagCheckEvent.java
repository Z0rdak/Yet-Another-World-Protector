package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraftforge.eventbus.api.Cancelable;
import net.minecraftforge.eventbus.api.Event;

import javax.annotation.Nullable;

// TODO: FlagCheck for 'nested' flags (when boolean flag works with true and false) like secondary tool or use portal
// TODO: Split in Event which holds the input data and is cancelable and in a result type which is used for processing flagMSg, etc
@Cancelable
public class FlagCheckEvent extends Event {

    private final DimensionalRegion dimRegion;
    @Nullable
    private final IMarkableRegion localRegion;
    private boolean isDenied;
    private boolean isDeniedInDim;
    private boolean isDeniedLocal;
    private final RegionFlag regionFlag;
    private IFlag flag;

    public FlagCheckEvent(DimensionalRegion dimRegion, IMarkableRegion localRegion, RegionFlag regionFlag) {
        this(dimRegion, localRegion, regionFlag, true, true, true);
    }

    public FlagCheckEvent(DimensionalRegion dimRegion, IMarkableRegion localRegion, RegionFlag regionFlag, boolean isDenied, boolean isDeniedLocal, boolean isDeniedInDim) {
        this.dimRegion = dimRegion;
        this.localRegion = localRegion;
        this.regionFlag = regionFlag;
        this.isDenied = isDenied;
        this.isDeniedLocal = isDeniedLocal;
        this.isDeniedInDim = isDeniedInDim;
    }

    public IFlag getFlag() {
        return flag;
    }

    public void setFlag(IFlag flag) {
        this.flag = flag;
    }

    public RegionFlag getRegionFlag() {
        return regionFlag;
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
}
