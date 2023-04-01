package de.z0rdak.yawp.util;

import de.z0rdak.yawp.core.stick.AbstractStick;
import de.z0rdak.yawp.core.stick.MarkerStick;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtList;
import net.minecraft.nbt.NbtString;
import net.minecraft.registry.RegistryKey;
import net.minecraft.text.MutableText;
import net.minecraft.text.Text;
import net.minecraft.world.World;

import java.util.Objects;

import static net.minecraft.util.Formatting.*;

public final class StickUtil {

    public static final String MARKED_BLOCKS = "blocks";
    public static final String VALID_AREA = "valid";
    public static final String AREA_TYPE = "type";
    public static final String DIM = "dim";
    public static final String TP_POS = "tp_pos";
    public static final String IS_TP_SET = "is_tp_set";
    public static final String STICK_ID = "stick-id";
    public static final String STICK_TYPE = "stick_type";
    private StickUtil() {
    }
    public static final String STICK = "stick";

    public static void applyEnchantmentGlint(ItemStack item) {
        NbtCompound dummy = new NbtCompound();
        dummy.putString("id", "");
        dummy.putInt("lvl", 1);
        NbtList enchantmentList = new NbtList();
        enchantmentList.add(dummy);
        item.setSubNbt("Enchantments", enchantmentList);
    }

    /**
     * Set init (default) nbt value for sticks
     *
     * @param stick stick item
     * @param type  stick type to create
     * @param dim   dimension tag to set for sticks
     */
    public static void initStickTag(ItemStack stick, StickType type, RegistryKey<World> dim) {
        NbtCompound itemTag = stick.hasNbt() ? stick.getNbt() : new NbtCompound();
        if (itemTag != null) {
            if (Objects.requireNonNull(type) == StickType.MARKER) {
                NbtCompound compoundNBT = new MarkerStick(dim).serializeNBT();
                itemTag.put(STICK, compoundNBT);
                stick.setNbt(itemTag);
            }
        }
    }

    public static ItemStack initMarkerNbt(ItemStack stack, StickType type, RegistryKey<World> dim) {
        stack.setCount(1);
        initStickTag(stack, type, dim);
        setStickName(stack, type);
        setStickToolTip(stack, type);
        applyEnchantmentGlint(stack);
        return stack;
    }

    public static boolean isVanillaStick(ItemStack itemStack) {
        return ItemStack.areItemsEqual(itemStack, Items.STICK.getDefaultStack());
    }

    public static AbstractStick getStick(ItemStack stick) throws StickException {
        if (stick.getNbt() != null && stick.hasNbt()) {
            if (stick.getNbt().contains(STICK)) {
                NbtCompound stickNbt = stick.getNbt().getCompound(STICK);
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
        throw new StickException("Invalid or missing NBT data for Stick '" + stick.toHoverableText().getString() + "'!");
    }

    public static StickType getStickType(ItemStack stick) {
        if (stick.getNbt() != null && stick.hasNbt()) {
            if (stick.getNbt().contains(STICK)) {
                NbtCompound stickNbt = stick.getNbt().getCompound(STICK);
                if (stickNbt.contains(STICK_TYPE)) {
                    return StickType.of(stickNbt.getString(STICK_TYPE));
                }
            }
        }
        return StickType.UNKNOWN;
    }

    public static NbtCompound getStickNBT(ItemStack stick) {
        if (stick.getNbt() != null && stick.hasNbt()
                && stick.getNbt().contains(STICK)) {
            return stick.getNbt().getCompound(STICK);
        } else {
            return null;
        }
    }

    public static void setStickName(ItemStack stick, StickType type) {
        String displayName = "";
        if (Objects.requireNonNull(type) == StickType.MARKER) {
            MarkerStick marker = new MarkerStick(getStickNBT(stick));
            String validFlag = marker.isValidArea() ? (GREEN + "*" + GOLD) : "";
            displayName = GOLD + type.stickName + " (" + marker.getAreaType().areaType + validFlag + ")";
        }
        stick.setCustomName(Text.literal((displayName)));
    }

    public static void setStickToolTip(ItemStack stick, StickType type) {
        if (Objects.requireNonNull(type) == StickType.MARKER) {
            setToolTip(stick, getMarkerToolTip());
        }
    }

    public static void setToolTip(ItemStack stack, NbtList loreNbt) {
        stack.getOrCreateSubNbt("display").put("Lore", loreNbt);
    }

    public static boolean hasNonNullTag(ItemStack itemStack) {
        return itemStack.hasNbt() && itemStack.getNbt() != null;
    }

    private static NbtList getMarkerToolTip() {
        NbtList lore = new NbtList();
        lore.add(buildLoreTextLine(Text.translatable("help.tooltip.stick.marker.simple.1"), "#ff4d4d"));
        lore.add(buildLoreTextLine(Text.translatable("help.tooltip.stick.marker.simple.2"), "#ff4d4d"));
        lore.add(buildLoreTextLine(Text.translatable("help.tooltip.stick.marker.simple.3").formatted(ITALIC), "#808080"));
        lore.add(buildLoreTextLine(Text.translatable("help.tooltip.stick.marker.simple.4").formatted(ITALIC), "#808080"));
        return lore;
    }

    private static NbtString buildLoreTextLine(String text, String hexColor) {
        return NbtString.of("{\"text\":\"" + text + "\", \"color\":\"" + hexColor + "\"}");
    }

    private static NbtString buildLoreTextLine(MutableText text, String hexColor) {
        return buildLoreTextLine(text.getString(), hexColor);
    }

}
