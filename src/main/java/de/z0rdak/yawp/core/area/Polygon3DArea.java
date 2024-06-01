package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.util.AreaUtil;
import de.z0rdak.yawp.util.constants.AreaNBT;
import net.minecraft.core.BlockPos;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.nbt.Tag;
import org.apache.commons.lang3.NotImplementedException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

public class Polygon3DArea extends AbstractArea {

    private List<BlockPos> positions;

    private Polygon3DArea() {
        super(AreaType.POLYGON_3D);
        this.positions = new ArrayList<>();
    }

    public Polygon3DArea(List<BlockPos> positions){
        this();
        this.positions = positions;
    }

    public Polygon3DArea(HolderLookup.Provider provider, CompoundTag nbt) {
        super(provider, nbt);
        this.deserializeNBT(provider, nbt);
    }

    public List<BlockPos> getPositions() {
        return Collections.unmodifiableList(positions);
    }

    @Override
    public boolean contains(BlockPos q) {
        return calcAngleSum(q, this.positions, this.positions.size()) == 2 * Math.PI;
    }

    /**
     * <a href="https://www.eecs.umich.edu/courses/eecs380/HANDOUTS/PROJ2/InsidePoly.html">...</a>
     * @param q
     * @param posList
     * @param n
     * @return
     */
    private static double calcAngleSum(BlockPos q, List<BlockPos> posList, int n){
        final double ESPILON = 0.0000001;
        double m1, m2, cosTheta, angleSum = 0;
        BlockPos p1, p2;
        for (int i = 0; i < n; i++) {
            p1 = posList.get(i).subtract(q);
            p2 =  posList.get((i + 1) % n).subtract(q);
            m1 = modulus(p1);
            m2 = modulus(p2);
            if (m1 * m2 <= ESPILON) {
                return 2 * Math.PI; /* We are on a node, consider this inside */
            } else {
                cosTheta = (p1.getX() * p2.getX() + p1.getY() * p2.getY() + p1.getZ() * p2.getZ() / (m1*m2));
            }
            angleSum += Math.acos(cosTheta);
        }
        return angleSum;
    }

    private static double modulus(BlockPos p){
        return Math.sqrt(p.getX()*p.getX() + p.getY()*p.getY() + p.getZ() * p.getZ());
    }

    @Override
    public CompoundTag serializeNBT(HolderLookup.Provider provider) {
        CompoundTag nbt = super.serializeNBT(provider);
        ListTag pointList = new ListTag();
        this.positions.forEach((point) -> {
            Tag pointNbt = NbtUtils.writeBlockPos(point);
            pointList.add(pointNbt);
        });
        nbt.put(AreaNBT.BLOCKS, pointList);
        return nbt;
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        this.positions.clear();
        this.deserializeNBT(provider, nbt);
        ListTag pointList = nbt.getList(AreaNBT.BLOCKS, Tag.TAG_COMPOUND);
        for (int i = 0; i < pointList.size(); i++) {
            BlockPos pos = AreaUtil.readBlockPos(pointList.getCompound(i));
            this.positions.add(pos);
        }
    }

    @Override
    public String toString() {
        throw new NotImplementedException("Missing toString");
    }

    @Override
    public List<BlockPos> getMarkedBlocks() {
        return this.positions;
    }

    @Override
    public Set<BlockPos> getHull() {
        throw new NotImplementedException("ChunkArea.getHull() not implemented yet");
    }


    @Override
    public boolean containsOther(IMarkableArea other) {
        throw new NotImplementedException("Not yet implemented");
    }

    @Override
    public boolean intersects(IMarkableArea other) {
        throw new NotImplementedException("Not yet implemented");
    }
}
