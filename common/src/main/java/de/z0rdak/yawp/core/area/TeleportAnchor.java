package de.z0rdak.yawp.core.area;

import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import de.z0rdak.yawp.core.INbtSerializable;
import de.z0rdak.yawp.util.NbtCompatHelper;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;

import static de.z0rdak.yawp.util.ChatComponentBuilder.tinyBlockPos;

public class TeleportAnchor implements INbtSerializable<CompoundTag> {

    public BlockPos getPos() {
        return pos;
    }

    public void setPos(BlockPos pos) {
        this.pos = pos;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    private BlockPos pos;
    private String name;
    // private Direction facing;
    // DisplayProperties display;

    public TeleportAnchor(BlockPos pos, String name) {
        this.pos = pos;
        this.name = name;
    }

    public TeleportAnchor(CompoundTag tag) {
        this.deserializeNBT(tag);
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag tag = new CompoundTag();
        tag.putString("name", this.name);
        tag.put("pos",  NbtUtils.writeBlockPos(this.pos));
        return tag;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        this.name = nbt.getString("name");
        this.pos = NbtCompatHelper.toBlockPos(nbt, "pos").orElseThrow();
    }
}