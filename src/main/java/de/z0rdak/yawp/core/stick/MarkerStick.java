package de.z0rdak.yawp.core.stick;

import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.util.StickType;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.Registries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.util.INBTSerializable;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class MarkerStick extends AbstractStick implements INBTSerializable<CompoundTag> {

    public static final String MARKED_BLOCKS = "blocks";
    public static final String VALID_AREA = "valid";
    public static final String AREA_TYPE = "type";
    public static final String DIM = "dim";
    public static final String TP_POS = "tp_pos";
    public static final String IS_TP_SET = "is_tp_set";

    private BlockPos teleportPos;
    private ResourceKey<Level> dimension;
    private AreaType areaType;
    private boolean isValidArea;
    private List<BlockPos> markedBlocks;

    public MarkerStick(AreaType areaType, boolean isValidArea, List<BlockPos> markedBlocks, ResourceKey<Level> dim) {
        this(areaType, isValidArea, markedBlocks, dim, null);
    }

    public MarkerStick(AreaType areaType, boolean isValidArea, List<BlockPos> markedBlocks, ResourceKey<Level> dim, BlockPos tpPos) {
        super(StickType.MARKER);
        this.areaType = areaType;
        this.isValidArea = isValidArea;
        this.markedBlocks = markedBlocks;
        this.dimension = dim;
        this.teleportPos = tpPos;
    }

    public MarkerStick(ResourceKey<Level> dim) {
        super(StickType.MARKER);
        this.areaType = AreaType.CUBOID;
        this.isValidArea = false;
        this.markedBlocks = new ArrayList<>();
        this.dimension = dim;
        this.teleportPos = null;
    }

    public MarkerStick(CompoundTag nbt) {
        super(StickType.MARKER);
        this.deserializeNBT(nbt);
    }

    public void cycleMode() {
        this.areaType = AreaType.values()[(this.areaType.ordinal() + 1) % AreaType.values().length];
        reset();
    }

    public void reset() {
        this.markedBlocks = new ArrayList<>();
        this.isValidArea = false;
        this.teleportPos = null;
    }

    public BlockPos getTeleportPos() {
        return teleportPos;
    }

    public void setTeleportPos(BlockPos teleportPos) {
        this.teleportPos = teleportPos;
    }

    public ResourceKey<Level> getDimension() {
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
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        // makes stick unique -> non-stackable
        // this could with a little code-rearrangement used to track which player used a specific stick
        // but why? Maybe sticks can store permissions in the future? This, of course, would be a potential risk for region owners
        nbt.putString("stick-id", UUID.randomUUID().toString());
        nbt.putBoolean(VALID_AREA, this.isValidArea);
        nbt.putString(AREA_TYPE, this.areaType.areaType);
        nbt.putString(DIM, this.dimension.location().toString());
        nbt.putBoolean(IS_TP_SET, this.teleportPos != null);
        if (this.teleportPos != null) {
            nbt.put(TP_POS, NbtUtils.writeBlockPos(this.teleportPos));
        }
        ListTag blocks = new ListTag();
        this.markedBlocks.forEach(block -> blocks.add(NbtUtils.writeBlockPos(block)));
        nbt.put(MARKED_BLOCKS, blocks);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.isValidArea = nbt.getBoolean(VALID_AREA);
        this.areaType = AreaType.of(nbt.getString(AREA_TYPE));
        boolean isTpSet = nbt.getBoolean(IS_TP_SET);
        if (isTpSet) {
            this.teleportPos = NbtUtils.readBlockPos(nbt.getCompound(TP_POS));
        }
        this.dimension = ResourceKey.create(Registries.DIMENSION, new ResourceLocation(nbt.getString(DIM)));
        ListTag markedBlocksNBT = nbt.getList(MARKED_BLOCKS, Tag.TAG_COMPOUND);
        this.markedBlocks = new ArrayList<>();
        markedBlocksNBT.forEach(block -> this.markedBlocks.add(NbtUtils.readBlockPos((CompoundTag) block)));
    }
}
