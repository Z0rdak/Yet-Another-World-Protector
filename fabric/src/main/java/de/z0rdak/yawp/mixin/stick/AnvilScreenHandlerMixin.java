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

// Note: this mixin is currently disabled (not added to yawp.mixins.json)
// TODO: Remove with WorldEdit integration feature
@Mixin(AnvilMenu.class)
public abstract class AnvilScreenHandlerMixin {

    @Inject(method = "onTake", at = @At("HEAD"), cancellable = true, allow = 1)
    private void onTakeOutput(Player player, ItemStack outputItem, CallbackInfo ci) {
        if (isServerSide(player.level())) {
            // Retrieve the input and output items from the anvil menu
            ItemStack inputItem = ((AnvilMenu) (Object) this).getSlot(0).getItem();
            ItemStack ingredientInput = ((AnvilMenu) (Object) this).getSlot(1).getItem();

            boolean isInputAndOutputStick = ItemStack.isSameItem(outputItem, Items.STICK.getDefaultInstance())
                    && ItemStack.isSameItem(inputItem, Items.STICK.getDefaultInstance());
            if (isInputAndOutputStick && ingredientInput.isEmpty()) {
                StickType type = StickType.of(outputItem.getDisplayName().getString());
                if (type != StickType.UNKNOWN) {
                    MarkerStickHandler.onCreateStick(player, inputItem, outputItem, type);
                    player.getInventory().setChanged();
                    ci.cancel();
                }
            }
        }
    }
}
