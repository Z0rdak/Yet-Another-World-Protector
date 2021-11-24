package de.z0rdak.regionshield.common.core.area;

import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.common.util.Constants;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

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

    public Polygon3DArea(CompoundNBT nbt) {
        this();
        this.deserializeNBT(nbt);
    }

    public List<BlockPos> getPositions() {
        return Collections.unmodifiableList(positions);
    }

    // TODO: validate
    @Override
    public boolean contains(BlockPos q) {
        return calcAngleSum(q, this.positions, this.positions.size()) == 2 * Math.PI;
    }

    /**
     * https://www.eecs.umich.edu/courses/eecs380/HANDOUTS/PROJ2/InsidePoly.html
     * @param q
     * @param posList
     * @param n
     * @return
     */
    private static double calcAngleSum(BlockPos q, List<BlockPos> posList, int n){
        final double ESPILON = 0.0000001;
        double m1, m2, costheta, angleSum = 0;
        BlockPos p1, p2;
        for (int i = 0; i < n; i++) {
            BlockPos posI = posList.get(i);
            int p1x = posI.getX() - q.getX();
            int p1y = posI.getY() - q.getY();
            int p1z = posI.getZ() - q.getZ();
            p1 = new BlockPos(p1x, p1y, p1z);
            posI = posList.get((i + 1) % n);
            int p2x = posI.getX() - q.getX();
            int p2y = posI.getY() - q.getY();
            int p2z = posI.getZ() - q.getZ();
            p2 = new BlockPos(p2x, p2y, p2z);

            m1 = modulus(p1);
            m2 = modulus(p2);

            if (m1 * m2 <= ESPILON) {
                return 2 * Math.PI; /* We are on a node, consider this inside */
            } else {
                costheta = (p1.getX() * p2.getX() + p1.getY() * p2.getY() + p1.getZ() * p2.getZ() / (m1*m2));
            }
            angleSum += Math.acos(costheta);
        }
        return angleSum;
    }

    private static double modulus(BlockPos p){
        return Math.sqrt(p.getX()*p.getX() + p.getY()*p.getY() + p.getZ() * p.getZ());
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        ListNBT pointList = new ListNBT();
        this.positions.forEach((point) -> {
            CompoundNBT pointNbt = NBTUtil.writeBlockPos(point);
            pointList.add(pointNbt);
        });
        nbt.put("points", pointList);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.positions.clear();
        ListNBT pointList = nbt.getList("points", Constants.NBT.TAG_COMPOUND);
        for (int i = 0; i < pointList.size(); i++) {
            BlockPos pos = NBTUtil.readBlockPos(pointList.getCompound(i));
            this.positions.add(pos);
        }
    }


}
