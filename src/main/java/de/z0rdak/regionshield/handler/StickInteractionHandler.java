package de.z0rdak.regionshield.handler;

import de.z0rdak.regionshield.RegionShield;
import de.z0rdak.regionshield.util.StickUtil;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.nbt.*;
import net.minecraft.util.text.*;
import net.minecraftforge.event.entity.player.AnvilRepairEvent;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import static de.z0rdak.regionshield.util.StickUtil.STICK_TYPE;

@Mod.EventBusSubscriber(modid = RegionShield.MODID)
public class StickInteractionHandler {

    @SubscribeEvent
    public static void onRightClickBlock(PlayerInteractEvent.RightClickBlock event){
        // TODO: retrieve stick used

        // TODO: check block and handle stick action accordingly

        // TODO: rendering and charge use needs to be implemented in stickitem mixin
    }


    @SubscribeEvent
    public static void onRightClickAir(PlayerInteractEvent.RightClickEmpty event){
        // TODO: retrieve stick used

        // TODO: cycle modes according to sticks
    }

    @SubscribeEvent
    public static void onLeftClickBlock(PlayerInteractEvent.LeftClickBlock event){

    }

    public static void onAttackPlayer(AttackEntityEvent event){
        // TODO: handle add players with region stick
    }


    /**
     * MAYBE: Rename only one stick and split stack
     * @param event
     */
    @SubscribeEvent
    public static void onStickRename(AnvilRepairEvent event) {
        PlayerEntity player = event.getPlayer();
        if (!player.getCommandSenderWorld().isClientSide) {
            ItemStack namedStick = event.getItemResult();
            if (!namedStick.isEmpty() && ItemStack.isSame(namedStick, Items.STICK.getDefaultInstance())
                    && ItemStack.isSame(event.getItemInput(), Items.STICK.getDefaultInstance())
                    && event.getIngredientInput().isEmpty()) {
                String stickName = namedStick.getDisplayName().getString();
                stickName = stickName.substring(1, stickName.length() - 1);
                StickUtil.StickType type = StickUtil.StickType.toStickType(stickName);
                if (type != null) {
                    updateStickTag(namedStick, type);
                    setStickNameAndTooltip(namedStick, type);
                    applyEnchantmentGlint(namedStick);
                }
            }
        }
    }

    private static void setStickNameAndTooltip(ItemStack stick, StickUtil.StickType type){
        String displayName = "";
        switch (type) {
            case REGION_STICK:
                displayName = TextFormatting.GREEN + type.stickName;
                setToolTip(stick, getRegionStickToolTip());
                break;
            case FLAG_STICK:
                displayName = TextFormatting.AQUA + type.stickName;
                setToolTip(stick, getFlagStickToolTip());
                break;
            case MARKER:
                displayName = TextFormatting.RED + type.stickName;
                setToolTip(stick, getMarkerToolTip());
                break;
            default:
                break;
        }
        stick.setHoverName(new StringTextComponent(displayName));
    }

    public static void setToolTip(ItemStack stack, ListNBT loreNbt){
        stack.getOrCreateTagElement("display").put("Lore", loreNbt);
    }

    // TODO
    private static ListNBT getMarkerToolTip(){
        ListNBT lore = new ListNBT();
        StringNBT simple1 = buildLoreTextLine(new TranslationTextComponent( "help.tooltip.stick.marker.simple.1"),"#ff0020");
        StringNBT simple2 = buildLoreTextLine(new TranslationTextComponent( "help.tooltip.stick.marker.simple.2"),"#ff0010");
        lore.add(simple1);
        lore.add(simple2);
        return lore;
    }

    // TODO
    private static ListNBT getFlagStickToolTip(){
        ListNBT lore = new ListNBT();
        StringNBT simple1 = buildLoreTextLine(new TranslationTextComponent( "help.tooltip.stick.flag-stick.simple.1"),"#ff0020");
        StringNBT simple2 = buildLoreTextLine(new TranslationTextComponent( "help.tooltip.stick.flag-stick.simple.2"),"#ff0010");
        lore.add(simple1);
        lore.add(simple2);
        return lore;
    }

    // TODO
    private static ListNBT getRegionStickToolTip(){
        ListNBT lore = new ListNBT();
        StringNBT simple1 = buildLoreTextLine(new TranslationTextComponent( "help.tooltip.stick.region-stick.simple.1"),"#ff0020");
        StringNBT simple2 = buildLoreTextLine(new TranslationTextComponent( "help.tooltip.stick.region-stick.simple.2"),"#ff0010");
        lore.add(simple1);
        lore.add(simple2);
        return lore;
    }

    private static StringNBT buildLoreTextLine(String text, String hexColor){
        return StringNBT.valueOf("{\"text\":\"" + text + "\", \"color\":\"" + hexColor + "\"}");
    }

    private static StringNBT buildLoreTextLine(IFormattableTextComponent text, String hexColor){
        return buildLoreTextLine(text.getString(), hexColor);
    }

    public static void applyEnchantmentGlint(ItemStack item){
        CompoundNBT dummy = new CompoundNBT();
        dummy.putString("id", "");
        dummy.putInt("lvl", 1);
        ListNBT enchantmentList = new ListNBT();
        enchantmentList.add(dummy);
        item.addTagElement("Enchantments", enchantmentList);
    }

    private static void updateStickTag(ItemStack stick, StickUtil.StickType type){
        if (stick == null) {
            RegionShield.LOGGER.warn("?");
            return;
        }
        if (stick.hasTag()){
            CompoundNBT itemTag = stick.getTag();
            if (!itemTag.contains(STICK_TYPE)){
                itemTag.putString(STICK_TYPE, type.stickName);
            }
        } else {
            CompoundNBT nbt = new CompoundNBT();
            nbt.putString(STICK_TYPE, type.stickName);
            stick.setTag(nbt);
        }
    }
}
