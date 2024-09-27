package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.IMarkableArea;
import net.minecraft.core.BlockPos;

/**
 * A mark-able region extends the general IProtectedRegion by allowing
 * to specify a certain area for the Region.
 * The area of the region is defined by a IMarkableArea instance.
 * Additionally, a mark-able region has a dimension it is located in.
 * <p>
 * A mark-able region also can be muted, has a priority to manage overlapping regions
 * and has a teleportation target.
 */
public interface IMarkableRegion extends IProtectedRegion {

    IMarkableArea getArea();

    void setArea(IMarkableArea area);

    boolean contains(BlockPos position);

    BlockPos getTpTarget();

    void setTpTarget(BlockPos pos);

    int getPriority();

    void setPriority(int priority);

    void rename(String newName);

}
