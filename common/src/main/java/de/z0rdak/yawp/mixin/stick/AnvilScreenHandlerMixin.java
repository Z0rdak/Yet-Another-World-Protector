package de.z0rdak.yawp.mixin.stick;

import de.z0rdak.yawp.handler.MarkerStickHandler;
import de.z0rdak.yawp.util.StickType;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AnvilMenu;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static de.z0rdak.yawp.handler.HandlerUtil.isServerSide;

// TODO: Remove with WorldEdit integration feature
@Mixin(AnvilMenu.class)
public abstract class AnvilScreenHandlerMixin {

    @Inject(method = "onTake", at = @At("HEAD"), cancellable = true, allow = 1)
    private void onTakeOutput(Player player, ItemStack outputItem, CallbackInfo ci) {
        if (isServerSide(player.level())) {
            try {
                AnvilMenu anvilMenu = (AnvilMenu) (Object) this;
                ItemStack inputItem = anvilMenu.getSlot(0).getItem();
                ItemStack ingredientInput = anvilMenu.getSlot(1).getItem();
                ItemStack stick = Items.STICK.getDefaultInstance();
                boolean isInputAndOutputStick = ItemStack.isSameItem(outputItem, stick)
                        && ItemStack.isSameItem(inputItem, stick);
                if (isInputAndOutputStick && ingredientInput.isEmpty()) {
                    MarkerStickHandler.onCreateStick(player, inputItem, outputItem);
                    player.getInventory().setChanged();
                    ci.cancel();
                }
            } catch (ClassCastException cce) {
                // Should not happen - if so we ignore it simply
            }
        }
    }
}
