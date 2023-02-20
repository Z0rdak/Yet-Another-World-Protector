package de.z0rdak.yawp.core.stick;

import de.z0rdak.yawp.core.INbtSerializable;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.util.StickType;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtElement;
import net.minecraft.nbt.NbtHelper;
import net.minecraft.nbt.NbtList;
import net.minecraft.registry.RegistryKeys;
import net.minecraft.util.Identifier;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.registry.RegistryKey;
import net.minecraft.world.World;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class MarkerStick extends AbstractStick implements INbtSerializable<NbtCompound> {

    public static final String MARKED_BLOCKS = "blocks";
    public static final String VALID_AREA = "valid";
    public static final String AREA_TYPE = "type";
    public static final String DIM = "dim";
    public static final String TP_POS = "tp_pos";
    public static final String IS_TP_SET = "is_tp_set";

    private BlockPos teleportPos;
    private RegistryKey<World> dimension;
    private AreaType areaType;
    private boolean isValidArea;
    private List<BlockPos> markedBlocks;

    public MarkerStick(AreaType areaType, boolean isValidArea, List<BlockPos> markedBlocks, RegistryKey<World> dim) {
        this(areaType, isValidArea, markedBlocks, dim, null);
    }

    public MarkerStick(AreaType areaType, boolean isValidArea, List<BlockPos> markedBlocks, RegistryKey<World> dim, BlockPos tpPos) {
        super(StickType.MARKER);
        this.areaType = areaType;
        this.isValidArea = isValidArea;
        this.markedBlocks = markedBlocks;
        this.dimension = dim;
        this.teleportPos = tpPos;
    }

    public MarkerStick(RegistryKey<World> dim) {
        super(StickType.MARKER);
        this.areaType = AreaType.CUBOID;
        this.isValidArea = false;
        this.markedBlocks = new ArrayList<>();
        this.dimension = dim;
        this.teleportPos = null;
    }

    public MarkerStick(NbtCompound nbt) {
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
    public NbtCompound serializeNBT() {
        NbtCompound nbt = super.serializeNBT();
        // makes stick unique -> non-stackable
        // this could with a little code-rearrangement used to track which player used a specific stick
        // but why? Maybe sticks can store permissions in the future? This, of course, would be a potential risk for region owners
        nbt.putString("stick-id", UUID.randomUUID().toString());
        nbt.putBoolean(VALID_AREA, this.isValidArea);
        nbt.putString(AREA_TYPE, this.areaType.areaType);
        nbt.putString(DIM, this.dimension.getValue().toString());
        nbt.putBoolean(IS_TP_SET, this.teleportPos != null);
        if (this.teleportPos != null) {
            nbt.put(TP_POS, NbtHelper.fromBlockPos(this.teleportPos));
        }
        NbtList blocks = new NbtList();
        this.markedBlocks.forEach(block -> blocks.add(NbtHelper.fromBlockPos(block)));
        nbt.put(MARKED_BLOCKS, blocks);
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
        this.isValidArea = nbt.getBoolean(VALID_AREA);
        this.areaType = AreaType.of(nbt.getString(AREA_TYPE));
        boolean isTpSet = nbt.getBoolean(IS_TP_SET);
        if (isTpSet) {
            this.teleportPos = NbtHelper.toBlockPos(nbt.getCompound(TP_POS));
        }
        this.dimension = RegistryKey.of(RegistryKeys.WORLD, new Identifier(nbt.getString(DIM)));
        NbtList markedBlocksNBT = nbt.getList(MARKED_BLOCKS, NbtElement.COMPOUND_TYPE);
        this.markedBlocks = new ArrayList<>();
        markedBlocksNBT.forEach(block -> this.markedBlocks.add(NbtHelper.toBlockPos((NbtCompound) block)));
    }
}
