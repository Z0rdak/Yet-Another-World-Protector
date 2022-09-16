package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.area.IMarkableArea;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.Level;

import java.util.Map;

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

   ResourceKey<Level> getDim();

   IMarkableArea getArea();

   void setArea(IMarkableArea area);

   boolean contains(BlockPos position);

   BlockPos getTpTarget();

   void setTpTarget(BlockPos pos);

   int getPriority();

   void setPriority(int priority);

   boolean isMuted();

   void setIsMuted(boolean isMuted);

   AbstractRegion getParent();

   void setParent(IProtectedRegion parent);

   Map<String, IMarkableRegion> getChildren();

   void addChild(IMarkableRegion child);

   void removeChild(IMarkableRegion child);

   boolean hasChild(IMarkableRegion child);
}
