package de.z0rdak.yawp.core.stick;

import de.z0rdak.yawp.constants.serialization.ItemNbtKeys;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.util.NbtCompatHelper;
import de.z0rdak.yawp.util.StickType;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.Registries;
import net.minecraft.nbt.*;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.Identifier;
import net.minecraft.world.level.Level;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class MarkerStick extends AbstractStick {

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
        nbt.putString(ItemNbtKeys.DIM, this.dimension.identifier().toString());
        ListTag blocks = new ListTag();
        this.markedBlocks.forEach(block -> blocks.add(NbtCompatHelper.asInts(block)));
        nbt.put(ItemNbtKeys.MARKED_BLOCKS, blocks);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.isValidArea = nbt.getBoolean(ItemNbtKeys.VALID_AREA).orElseThrow();
        this.areaType = AreaType.of(nbt.getString(ItemNbtKeys.AREA_TYPE).orElseThrow());
        this.dimension = ResourceKey.create(Registries.DIMENSION, Identifier.parse(nbt.getString(ItemNbtKeys.DIM).orElseThrow()));
        ListTag markedBlocksNBT = (ListTag) nbt.get(ItemNbtKeys.MARKED_BLOCKS);
        if (markedBlocksNBT != null) {
            this.markedBlocks = new ArrayList<>(this.areaType.maxBlocks);
            for (int i = 0; i < markedBlocksNBT.size(); i++) {
                int[] intArray = markedBlocksNBT.getIntArray(i).orElseThrow();
                NbtCompatHelper.asBlockPos(new IntArrayTag(intArray)).ifPresent(pos -> this.markedBlocks.add(pos));
            }
        }      
    }
}
