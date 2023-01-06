package de.z0rdak.yawp.util;

import de.z0rdak.yawp.core.stick.AbstractStick;
import de.z0rdak.yawp.core.stick.MarkerStick;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraft.nbt.StringNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;

import java.util.Objects;

public final class StickUtil {

    private StickUtil() {
    }

    public static final String STICK_TYPE = "stick_type";
    public static final String STICK = "stick";

    public static void applyEnchantmentGlint(ItemStack item) {
        CompoundNBT dummy = new CompoundNBT();
        dummy.putString("id", "");
        dummy.putInt("lvl", 1);
        ListNBT enchantmentList = new ListNBT();
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
    public static void initStickTag(ItemStack stick, StickType type, RegistryKey<World> dim) {
        CompoundNBT itemTag = stick.hasTag() ? stick.getTag() : new CompoundNBT();
        if (itemTag != null) {
            if (Objects.requireNonNull(type) == StickType.MARKER) {
                CompoundNBT compoundNBT = new MarkerStick(dim).serializeNBT();
                itemTag.put(STICK, compoundNBT);
                stick.setTag(itemTag);
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
        return itemStack.getItem().getDefaultInstance().getDescriptionId().equals(Items.STICK.getDescriptionId());
    }

    public static AbstractStick getStick(ItemStack stick) throws StickException {
        if (stick.getTag() != null && stick.hasTag()) {
            if (stick.getTag().contains(STICK)) {
                CompoundNBT stickNbt = stick.getTag().getCompound(STICK);
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
                CompoundNBT stickNbt = stick.getTag().getCompound(STICK);
                if (stickNbt.contains(STICK_TYPE)) {
                    return StickType.of(stickNbt.getString(STICK_TYPE));
                }
            }
        }
        return StickType.UNKNOWN;
    }

    public static CompoundNBT getStickNBT(ItemStack stick) {
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
            String validFlag = marker.isValidArea() ? (TextFormatting.GREEN + "*" + TextFormatting.GOLD) : "";
            displayName = TextFormatting.GOLD + type.stickName + " (" + marker.getAreaType().areaType + "" + validFlag + ")";
        }
        stick.setHoverName(new StringTextComponent(displayName));
    }

    public static void setStickToolTip(ItemStack stick, StickType type) {
        if (Objects.requireNonNull(type) == StickType.MARKER) {
            setToolTip(stick, getMarkerToolTip());
        }
    }

    public static void setToolTip(ItemStack stack, ListNBT loreNbt) {
        stack.getOrCreateTagElement("display").put("Lore", loreNbt);
    }

    public static boolean hasNonNullTag(ItemStack itemStack) {
        return itemStack.hasTag() && itemStack.getTag() != null;
    }

    private static ListNBT getMarkerToolTip() {
        ListNBT lore = new ListNBT();
        lore.add(buildLoreTextLine(new TranslationTextComponent("help.tooltip.stick.marker.simple.1"), "#ff4d4d"));
        lore.add(buildLoreTextLine(new TranslationTextComponent("help.tooltip.stick.marker.simple.2"), "#ff4d4d"));
        lore.add(buildLoreTextLine(new StringTextComponent(TextFormatting.ITALIC + "").append(new TranslationTextComponent("help.tooltip.stick.marker.simple.3")), "#808080"));
        lore.add(buildLoreTextLine(new StringTextComponent(TextFormatting.ITALIC + "").append(new TranslationTextComponent("help.tooltip.stick.marker.simple.4")), "#808080"));
        return lore;
    }

    private static StringNBT buildLoreTextLine(String text, String hexColor) {
        return StringNBT.valueOf("{\"text\":\"" + text + "\", \"color\":\"" + hexColor + "\"}");
    }

    private static StringNBT buildLoreTextLine(IFormattableTextComponent text, String hexColor) {
        return buildLoreTextLine(text.getString(), hexColor);
    }

}
