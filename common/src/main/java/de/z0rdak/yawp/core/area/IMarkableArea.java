package de.z0rdak.yawp.core.area;

import net.minecraft.core.BlockPos;

import java.util.Set;

/**
 * IMarkableArea provides an interface for different types of areas.
 * The most basic area would be a simple AxisAlignedBB.
 * This type of mark-able area is already implemented in the class CuboidArea.
 */
public interface IMarkableArea {

    boolean contains(BlockPos pos);

    AreaType getAreaType();

    Set<BlockPos> markedBlocks();

    /**
     * Set containing all block positions which represent the outer hull of the marked area.
     * The hull is the solid outer perimeter of the marked area.
     */
    Set<BlockPos> getHull();

    /**
     * Set containing all block positions which represent the outer frame of the marked area.
     * The frame is the minimum required blocks to give a rough outline o the marked area
     */
    Set<BlockPos> getFrame();
    Set<BlockPos> getMinimalOutline();

    boolean containsOther(IMarkableArea other);

    boolean intersects(IMarkableArea other);

    MarkedAreaType<?> getType();


    BlockDisplayProperties getDisplay();

    void updateDisplay(BlockDisplayProperties properties);
}
