package de.z0rdak.yawp.core.stick;

import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.util.StickType;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.registry.Registry;
import net.minecraft.world.World;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.common.util.INBTSerializable;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static de.z0rdak.yawp.util.StickUtil.*;

public class MarkerStick extends AbstractStick implements INBTSerializable<CompoundNBT> {

    private BlockPos teleportPos;
    private RegistryKey<World> dimension;
    private AreaType areaType;
    private boolean isValidArea;
    private List<BlockPos> markedBlocks;

    public MarkerStick(RegistryKey<World> dim) {
        super(StickType.MARKER);
        this.areaType = AreaType.CUBOID;
        this.isValidArea = false;
        this.markedBlocks = new ArrayList<>(this.areaType.maxBlocks);
        this.dimension = dim;
        this.teleportPos = null;
    }

    public MarkerStick(CompoundNBT nbt) {
        super(StickType.MARKER);
        this.deserializeNBT(nbt);
    }

    public void cycleMode() {
        this.areaType = AreaType.values()[(this.areaType.ordinal() + 1) % AreaType.values().length];
        reset();
    }

    public void reset() {
        this.markedBlocks = new ArrayList<>(this.areaType.maxBlocks);
        this.isValidArea = false;
        this.teleportPos = null;
    }

    public BlockPos getTeleportPos() {
        return teleportPos;
    }

    public void setTeleportPos(BlockPos teleportPos) {
        this.teleportPos = teleportPos;
    }

    public RegistryKey<World> getDimension() {
        return dimension;
    }

    public boolean checkValidArea() {
        int numMarkedBlocks = markedBlocks.size();
        if (markedBlocks.isEmpty() || areaType.neededBlocks == -1) {
            return false;
        }
        // check for cylinder, sphere and cuboid
        boolean exactlyEnoughBlocks = numMarkedBlocks == areaType.neededBlocks && numMarkedBlocks == areaType.maxBlocks;
        // check for polygon and prism
        boolean minBlocks = numMarkedBlocks >= areaType.neededBlocks && numMarkedBlocks <= areaType.maxBlocks;
        this.isValidArea = exactlyEnoughBlocks || minBlocks;
        return this.isValidArea;
    }

    public AreaType getAreaType() {
        return areaType;
    }

    public void setAreaType(AreaType areaType) {
        this.areaType = areaType;
    }

    public boolean isValidArea() {
        return isValidArea;
    }

    public List<BlockPos> getMarkedBlocks() {
        return markedBlocks;
    }

    public void addMarkedBlock(BlockPos pos) {
        int index = markedBlocks.size() % areaType.maxBlocks;
        this.markedBlocks.add(index, pos);
        if (markedBlocks.size() > areaType.maxBlocks) {
            markedBlocks.remove(areaType.maxBlocks);
        }
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.putString(STICK_ID, UUID.randomUUID().toString());
        nbt.putBoolean(VALID_AREA, this.isValidArea);
        nbt.putString(AREA_TYPE, this.areaType.areaType);
        nbt.putString(DIM, this.dimension.location().toString());
        nbt.putBoolean(IS_TP_SET, this.teleportPos != null);
        if (this.teleportPos != null) {
            nbt.put(TP_POS, NBTUtil.writeBlockPos(this.teleportPos));
        }
        ListNBT blocks = new ListNBT();
        this.markedBlocks.forEach(block -> blocks.add(NBTUtil.writeBlockPos(block)));
        nbt.put(MARKED_BLOCKS, blocks);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.isValidArea = nbt.getBoolean(VALID_AREA);
        this.areaType = AreaType.of(nbt.getString(AREA_TYPE));
        boolean isTpSet = nbt.getBoolean(IS_TP_SET);
        if (isTpSet) {
            this.teleportPos = NBTUtil.readBlockPos(nbt.getCompound(TP_POS));
        }
        this.dimension = RegistryKey.create(Registry.DIMENSION_REGISTRY,
                new ResourceLocation(nbt.getString(DIM)));
        ListNBT markedBlocksNBT = nbt.getList(MARKED_BLOCKS, Constants.NBT.TAG_COMPOUND);
        this.markedBlocks = new ArrayList<>(this.areaType.maxBlocks);
        markedBlocksNBT.forEach(block -> this.markedBlocks.add(NBTUtil.readBlockPos((CompoundNBT) block)));
    }
}
