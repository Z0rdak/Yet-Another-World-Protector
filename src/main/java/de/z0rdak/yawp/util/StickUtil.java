package de.z0rdak.yawp.util;

import de.z0rdak.yawp.core.stick.AbstractStick;
import de.z0rdak.yawp.core.stick.MarkerStick;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.StringTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.Level;

import java.util.Objects;

import static net.minecraft.ChatFormatting.*;

public final class StickUtil {

    private StickUtil() {
    }

    public static final String STICK_TYPE = "stick_type";
    public static final String STICK = "stick";

    public static void applyEnchantmentGlint(ItemStack item) {
        CompoundTag dummy = new CompoundTag();
        dummy.putString("id", "");
        dummy.putInt("lvl", 1);
        ListTag enchantmentList = new ListTag();
        enchantmentList.add(dummy);
        item.addTagElement("Enchantments", enchantmentList);
    }

    /**
     * Set init (default) nbt value for sticks
     *
     * @param stick stick item
     * @param type  stick type to create
     * @param dim   dimension tag to set for sticks
     */
    public static void initStickTag(ItemStack stick, StickType type, ResourceKey<Level> dim) {
        CompoundTag itemTag = stick.hasTag() ? stick.getTag() : new CompoundTag();
        if (itemTag != null) {
            if (Objects.requireNonNull(type) == StickType.MARKER) {
                CompoundTag compoundNBT = new MarkerStick(dim).serializeNBT();
                itemTag.put(STICK, compoundNBT);
                stick.setTag(itemTag);
            }
        }
    }

    public static ItemStack initMarkerNbt(ItemStack stack, StickType type, ResourceKey<Level> dim) {
        stack.setCount(1);
        initStickTag(stack, type, dim);
        setStickName(stack, type);
        setStickToolTip(stack, type);
        applyEnchantmentGlint(stack);
        return stack;
    }

    public static boolean isVanillaStick(ItemStack itemStack) {
        return itemStack.getItem().getDefaultInstance().getDescriptionId().equals(Items.STICK.getDescriptionId());
    }

    public static AbstractStick getStick(ItemStack stick) throws StickException {
        if (stick.getTag() != null && stick.hasTag()) {
            if (stick.getTag().contains(STICK)) {
                CompoundTag stickNbt = stick.getTag().getCompound(STICK);
                StickType type = StickType.of(stickNbt.getString(STICK_TYPE));
                switch (type) {
                    case MARKER:
                        return new MarkerStick(stickNbt);
                    case UNKNOWN:
                    default:
                        throw new StickException("Unknown stick type: '" + type + "'!");
                }
            }
        }
        throw new StickException("Invalid or missing NBT data for Stick '" + stick.getDisplayName().getString() + "'!");
    }

    public static StickType getStickType(ItemStack stick) {
        if (stick.getTag() != null && stick.hasTag()) {
            if (stick.getTag().contains(STICK)) {
                CompoundTag stickNbt = stick.getTag().getCompound(STICK);
                if (stickNbt.contains(STICK_TYPE)) {
                    return StickType.of(stickNbt.getString(STICK_TYPE));
                }
            }
        }
        return StickType.UNKNOWN;
    }

    public static CompoundTag getStickNBT(ItemStack stick) {
        if (stick.getTag() != null && stick.hasTag()
                && stick.getTag().contains(STICK)) {
            return stick.getTag().getCompound(STICK);
        } else {
            return null;
        }
    }

    public static void setStickName(ItemStack stick, StickType type) {
        String displayName = "";
        if (Objects.requireNonNull(type) == StickType.MARKER) {
            MarkerStick marker = new MarkerStick(getStickNBT(stick));
            String validFlag = marker.isValidArea() ? (GREEN + "*" + GOLD) : "";
            displayName = GOLD + type.stickName + " (" + marker.getAreaType().areaType + "" + validFlag + ")";
        }
        stick.setHoverName(Component.literal(displayName));
    }

    public static void setStickToolTip(ItemStack stick, StickType type) {
        if (Objects.requireNonNull(type) == StickType.MARKER) {
            setToolTip(stick, getMarkerToolTip());
        }
    }

    public static void setToolTip(ItemStack stack, ListTag loreNbt) {
        stack.getOrCreateTagElement("display").put("Lore", loreNbt);
    }

    public static boolean hasNonNullTag(ItemStack itemStack) {
        return itemStack.hasTag() && itemStack.getTag() != null;
    }

    private static ListTag getMarkerToolTip() {
        ListTag lore = new ListTag();
        lore.add(buildLoreTextLine(Component.translatable("Used to mark a new region."), "#ff4d4d"));
        lore.add(buildLoreTextLine(Component.translatable("Keep the Region Marker in your hand while creating a region!"), "#ff4d4d"));
        lore.add(buildLoreTextLine(Component.translatable("Mark a (Cuboid) region by right-clicking the diagonal opposite corner blocks.").withStyle(ITALIC), "#808080"));
        lore.add(buildLoreTextLine(Component.translatable("The green star on the RegionMarker indicates a valid area.").withStyle(ITALIC), "#808080"));
        return lore;
    }

    private static StringTag buildLoreTextLine(String text, String hexColor) {
        return StringTag.valueOf("{\"text\":\"" + text + "\", \"color\":\"" + hexColor + "\"}");
    }

    private static StringTag buildLoreTextLine(MutableComponent text, String hexColor) {
        return buildLoreTextLine(text.getString(), hexColor);
    }

}
