package de.z0rdak.regionshield.common.core.area;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.vector.Vector3d;

import static de.z0rdak.regionshield.common.core.area.AreaUtil.*;

public class CylinderArea extends CenteredArea {

    private CylinderArea() {
        super(AreaType.CYLINDER);
    }

    private BlockPos radiusPoint;
    private int height;
    private int radius;

    // TODO: validate
    public CylinderArea(BlockPos centerPos, BlockPos scopePos){
        super(centerPos, AreaType.CYLINDER);
        this.radiusPoint = scopePos;
        int maxDist = Math.max(scopePos.getZ(), scopePos.getX());
        BlockPos radiusPos = new BlockPos(maxDist, scopePos.getY(), maxDist);
        this.radius = (int) distance(centerPos, radiusPos);
        this.height = (int) distance(centerPos,
                new BlockPos(centerPos.getX(), scopePos.getY(), centerPos.getZ()));
    }

    public CylinderArea(CompoundNBT nbt){
        this();
        this.deserializeNBT(nbt);
    }

    public Vector3d getCenter() {
        return new Vector3d(this.center.getX(), this.center.getY(), this.center.getZ());
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
        CompoundNBT nbt = super.serializeNBT();
        nbt.putInt("radius", this.radius);
        nbt.putInt("height", this.height);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.height = nbt.getInt("height");
        this.radius = nbt.getInt("radius");
    }


}
