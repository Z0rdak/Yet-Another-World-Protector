package de.z0rdak.regionshield.common.core.area;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.util.math.BlockPos;

import static de.z0rdak.regionshield.common.core.area.AreaUtil.*;

public class CylinderArea extends AbstractArea {

    private CylinderArea() {
        super(AreaType.CYLINDER);
    }

    private BlockPos centerP;
    private BlockPos radiusPoint;
    private int height;
    private int radius;

    // TODO: validate
    public CylinderArea(BlockPos centerPos, BlockPos scopePos){
        this();
        this.centerP = centerPos;
        this.radiusPoint = scopePos;
        int maxDist = Math.max(scopePos.getZ(), scopePos.getX());
        BlockPos radiusPos = new BlockPos(maxDist, scopePos.getY(), maxDist);
        this.radius = (int) SphereArea.distance(centerPos, radiusPos);
        this.height = (int) SphereArea.distance(centerPos,
                new BlockPos(centerPos.getX(), scopePos.getY(), centerPos.getZ()));
    }

    public CylinderArea(CompoundNBT nbt){
        this();
        this.deserializeNBT(nbt);
    }

    public BlockPos getCenter() {
        return new BlockPos(this.centerP);
    }

    public BlockPos getScopePoint() {
        return new BlockPos(this.radiusPoint);
    }

    public int getHeight() {
        return height;
    }

    public int getRadius() {
        return radius;
    }

    /***
     * https://stackoverflow.com/questions/47932955/how-to-check-if-a-3d-point-is-inside-a-cylinder
     * @param pos
     * @return
     */
    @Override
    public boolean contains(BlockPos pos) {
        return false;
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.put("center", NBTUtil.writeBlockPos(this.centerP));
        nbt.putInt("radius", this.radius);
        nbt.putInt("height", this.height);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.centerP = NBTUtil.readBlockPos(nbt.getCompound("center"));
        this.height = nbt.getInt("height");
        this.radius = nbt.getInt("radius");
    }


}
