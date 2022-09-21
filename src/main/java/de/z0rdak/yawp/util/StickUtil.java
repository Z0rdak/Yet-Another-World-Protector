package de.z0rdak.yawp.util;

import de.z0rdak.yawp.core.stick.AbstractStick;
import de.z0rdak.yawp.core.stick.FlagStick;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.core.stick.RegionStick;
import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.StringTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.Level;

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
        CompoundTag itemTag = new CompoundTag();
        if (stick.hasTag()) {
            itemTag = stick.getTag();
        }
        if (itemTag != null) {
            switch (type) {
                case MARKER:
                    itemTag.put(STICK, new MarkerStick(dim).serializeNBT());
                    break;
                case FLAG_STICK:
                    itemTag.put(STICK, new FlagStick().serializeNBT());
                    break;
                case REGION_STICK:
                    itemTag.put(STICK, new RegionStick().serializeNBT());
                    break;
                default:
                    break;
            }
        }
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
                    case FLAG_STICK:
                        return new FlagStick(stickNbt);
                    case REGION_STICK:
                        return new RegionStick(stickNbt);
                    case UNKNOWN:
                    default:
                        throw new StickException();
                }
            }
        }
        throw new StickException();
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
        switch (type) {
            case REGION_STICK:
                displayName = ChatFormatting.GREEN + type.stickName;
                break;
            case FLAG_STICK:
                displayName = ChatFormatting.AQUA + type.stickName;
                break;
            case MARKER:
                MarkerStick marker = new MarkerStick(stick.getTag().getCompound(STICK));
                String validFlag = marker.isValidArea() ? (ChatFormatting.GREEN + "*" + ChatFormatting.GOLD) : "";
                displayName = ChatFormatting.GOLD + type.stickName + " (" + marker.getAreaType().areaType + "" + validFlag + ")";
                break;
            default:
                break;
        }
        stick.setHoverName(Component.literal(displayName));
    }

    public static void setStickToolTip(ItemStack stick, StickType type) {
        switch (type) {
            case REGION_STICK:
                setToolTip(stick, getRegionStickToolTip());
                break;
            case FLAG_STICK:
                setToolTip(stick, getFlagStickToolTip());
                break;
            case MARKER:
                setToolTip(stick, getMarkerToolTip());
                break;
            default:
                break;
        }
    }

    public static void setToolTip(ItemStack stack, ListTag loreNbt) {
        stack.getOrCreateTagElement("display").put("Lore", loreNbt);
    }

    // TODO
    private static ListTag getMarkerToolTip() {
        ListTag lore = new ListTag();
        StringTag simple1 = buildLoreTextLine(Component.translatable("help.tooltip.stick.marker.simple.1"), "#ff0020");
        StringTag simple2 = buildLoreTextLine(Component.translatable("help.tooltip.stick.marker.simple.2"), "#ff0010");
        lore.add(simple1);
        lore.add(simple2);
        return lore;
    }

    // TODO
    private static ListTag getFlagStickToolTip() {
        ListTag lore = new ListTag();
        StringTag simple1 = buildLoreTextLine(Component.translatable("help.tooltip.stick.flag-stick.simple.1"), "#ff0020");
        StringTag simple2 = buildLoreTextLine(Component.translatable("help.tooltip.stick.flag-stick.simple.2"), "#ff0010");
        lore.add(simple1);
        lore.add(simple2);
        return lore;
    }

    // TODO
    private static ListTag getRegionStickToolTip() {
        ListTag lore = new ListTag();
        StringTag simple1 = buildLoreTextLine(Component.translatable("help.tooltip.stick.region-stick.simple.1"), "#ff0020");
        StringTag simple2 = buildLoreTextLine(Component.translatable("help.tooltip.stick.region-stick.simple.2"), "#ff0010");
        lore.add(simple1);
        lore.add(simple2);
        return lore;
    }

    private static StringTag buildLoreTextLine(String text, String hexColor) {
        return StringTag.valueOf("{\"text\":\"" + text + "\", \"color\":\"" + hexColor + "\"}");
    }

    private static StringTag buildLoreTextLine(MutableComponent text, String hexColor) {
        return buildLoreTextLine(text.getString(), hexColor);
    }

}
