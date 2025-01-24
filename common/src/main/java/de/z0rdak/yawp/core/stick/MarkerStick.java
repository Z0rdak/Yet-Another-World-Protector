package de.z0rdak.yawp.core.stick;

import de.z0rdak.yawp.constants.serialization.ItemNbtKeys;
import de.z0rdak.yawp.core.INbtSerializable;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.util.NbtCompatHelper;
import de.z0rdak.yawp.util.StickType;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.Registries;
import net.minecraft.nbt.*;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.Level;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class MarkerStick extends AbstractStick implements INbtSerializable<CompoundTag> {

    private BlockPos teleportPos;
    private ResourceKey<Level> dimension;
    private AreaType areaType;
    private boolean isValidArea;
    private List<BlockPos> markedBlocks;

    public MarkerStick(ResourceKey<Level> dim) {
        super(StickType.MARKER);
        this.areaType = AreaType.CUBOID;
        this.isValidArea = false;
        this.markedBlocks = new ArrayList<>(this.areaType.maxBlocks);
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
        nbt.putString(ItemNbtKeys.STICK_ID, UUID.randomUUID().toString());
        nbt.putBoolean(ItemNbtKeys.VALID_AREA, this.isValidArea);
        nbt.putString(ItemNbtKeys.AREA_TYPE, this.areaType.areaType);
        nbt.putString(ItemNbtKeys.DIM, this.dimension.location().toString());
        nbt.putBoolean(ItemNbtKeys.IS_TP_SET, this.teleportPos != null);
        if (this.teleportPos != null) {
            nbt.put(ItemNbtKeys.TP_POS, NbtUtils.writeBlockPos(this.teleportPos));
        }
        ListTag blocks = new ListTag();
        this.markedBlocks.forEach(block -> blocks.add(NbtUtils.writeBlockPos(block)));
        nbt.put(ItemNbtKeys.MARKED_BLOCKS, blocks);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.isValidArea = nbt.getBoolean(ItemNbtKeys.VALID_AREA);
        this.areaType = AreaType.of(nbt.getString(ItemNbtKeys.AREA_TYPE));
        boolean isTpSet = nbt.getBoolean(ItemNbtKeys.IS_TP_SET);
        if (isTpSet) {
            this.teleportPos = NbtCompatHelper.toBlockPos(nbt, ItemNbtKeys.TP_POS).orElse(null);
        }
        this.dimension = ResourceKey.create(Registries.DIMENSION, ResourceLocation.parse(nbt.getString(ItemNbtKeys.DIM)));
        ListTag markedBlocksNBT = (ListTag) nbt.get(ItemNbtKeys.MARKED_BLOCKS);
        if (markedBlocksNBT != null) {
            this.markedBlocks = new ArrayList<>(this.areaType.maxBlocks);
            for (int i = 0; i < markedBlocksNBT.size(); i++) {
                int[] intArray = markedBlocksNBT.getIntArray(i);
                IntArrayTag intArrayTag = new IntArrayTag(intArray);
                NbtCompatHelper.toBlockPos(intArrayTag).ifPresent(pos -> this.markedBlocks.add(pos));
            }
        }      
    }
}
